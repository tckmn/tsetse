{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module GameUtil where

import Control.Applicative
import Data.Aeson
import Data.Functor
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import GM
import Types
import Util
import qualified Data.HashMap.Strict as M

-- functions for accessing games without pain

renderGameList :: ServerState -> GMOutMsg
renderGameList s = GameList $ map runDesc . reverse . sortOn (_creation . snd) $ M.toList (s^.games)
    where runDesc (gid, g@GeneralGame{..}) =
            let (gtype, gdesc) = desc _game
             in object [ "gid"      .> gid
                       , "gtype"    .> gtype
                       , "gdesc"    .> gdesc
                       , "uname"    .> (s^.byUid _creator.uname)
                       , "creator"  .> _creator
                       , "creation" .> _creation
                       , "dead"     .> _dead
                       , "conf"     .> toJSON _gconf
                       ]

runGameList :: ServerState -> IO ()
runGameList s = broadcastWS (s^..byGid (-1)) (renderGameList s)

runGameType :: Client -> ServerState -> IO ()
runGameType c s = sendWS c $ case (s^.cgame c) of
                               Just GeneralGame{..} -> GameType (desc _game ^. _1) (toJSON _gconf)
                               Nothing -> GameType "" Null

runCatchup :: Client -> ServerState -> IO ()
runCatchup c s = case s^.cgame c of
                   Just GeneralGame{..} -> runGameIO catchup (c, s, _gconf) _game $> ()
                   Nothing -> sendWS c (renderGameList s)

runRecv :: Client -> ServerState -> Text -> IO (ServerState, PostAction)
runRecv c s msg = do
    case s^.cgame c of
      Just g@GeneralGame{..} -> do
          let gio = fromMaybe empty $ recvT msg
          (post, g') <- runGameIO gio (c, s, _gconf) _game
          return $ (s & cgame c .~ Just GeneralGame { _creator
                                                    , _creation
                                                    , _dead
                                                    , _gconf
                                                    , _game = g' }, fromMaybe Done post)
      Nothing -> return (s, Done)

runUserlist :: Client -> ServerState -> IO ServerState
runUserlist c s = case s^.cgame c of
                    Just GeneralGame{..} -> runGameIO userlist (c, s, _gconf) _game $> s
                    Nothing -> return s

runDeletable :: ServerState -> GameId -> Bool
runDeletable s g = case s^.games.at g of
                    Just GeneralGame{..} -> deletable _game
                    Nothing -> False

-- common game monad tasks

send :: ToJSON a => a -> GameIO g ()
send msg = do
    conn <- view $ rclient.conn
    liftIO $ sendWS conn msg

broadcast :: ToJSON a => a -> GameIO g ()
broadcast msg = do
    g <- view $ rclient.gid
    s <- view $ rserver
    liftIO $ broadcastWS (s^..byGid g) msg

checkpwd :: Text -> GameIO g ()
checkpwd check = do
    pwd <- view $ rserver.password
    guard $ check == pwd
