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
import qualified Data.HashMap.Strict as M

-- functions for accessing games without pain

renderGameList :: ServerState -> GMOutMsg
renderGameList s = GameList . reverse . sortOn (^._2._4) $
        (\(gid, g) -> (gid, runDesc g, g^.dead)) <$> M.toList (s^.games)
    where runDesc GeneralGame{..} = let (a, b) = desc _game
                                     in (a, b, s^.byUid _creator.uname, _creation)

runGameList :: ServerState -> IO ()
runGameList s = broadcastWS (s^..byGid (-1)) (renderGameList s)

runGameType :: Client -> ServerState -> IO ()
runGameType c s = sendWS c . GameType $ case (s^.cgame c) of
                                          Just GeneralGame{..} -> desc _game ^. _1
                                          Nothing -> ""

runCatchup :: Client -> ServerState -> IO ()
runCatchup c s = case s^.cgame c of
                   Just GeneralGame{..} -> runGameIO catchup (c, s) _game $> ()
                   Nothing -> sendWS c (renderGameList s)

runRecv :: Client -> ServerState -> Text -> IO (ServerState, PostAction)
runRecv c s msg = do
    case s^.cgame c of
      Just g@GeneralGame{..} -> do
          let gio = fromMaybe empty $ recvT msg
          (post, g') <- runGameIO gio (c, s) _game
          return $ (s & cgame c .~ Just GeneralGame { _creator
                                                    , _creation
                                                    , _dead
                                                    , _gconf
                                                    , _game = g' }, fromMaybe Done post)
      Nothing -> return (s, Done)

runUserlist :: Client -> ServerState -> IO ServerState
runUserlist c s = case s^.cgame c of
                    Just GeneralGame{..} -> runGameIO userlist (c, s) _game $> s
                    Nothing -> return s

-- common game monad tasks

send :: ToJSON a => a -> GameIO g ()
send msg = do
    conn <- view $ _1.conn
    liftIO $ sendWS conn msg

broadcast :: ToJSON a => a -> GameIO g ()
broadcast msg = do
    g <- view $ _1.gid
    s <- view $ _2
    liftIO $ broadcastWS (s^..byGid g) msg

checkpwd :: Text -> GameIO g ()
checkpwd check = do
    pwd <- view $ _2.password
    guard $ check == pwd
