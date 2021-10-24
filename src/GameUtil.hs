{-# LANGUAGE RecordWildCards #-}

module GameUtil where

import Control.Concurrent
import Data.Aeson
import Data.Functor
import Data.Maybe (fromMaybe)
import GM
import Types
import qualified Data.HashMap.Strict as M

-- functions for accessing games without pain

runGameList :: Client -> ServerState -> IO ()
runGameList c s = sendWS (c^.conn) . GameList $ (_2 %~ runDesc) <$> M.toList (s^.games)
    where runDesc (GeneralGame g) = desc g

runCatchup :: Client -> ServerState -> IO ()
runCatchup c s = case s^.cgame c of
                   Just (GeneralGame g) -> runGameIO catchup (c, s) g $> ()
                   Nothing -> runGameList c s

runRecv :: Client -> MVar ServerState -> Text -> IO ()
runRecv c state msg = do
    mg <- withMVar state $ return . view (cgame c)
    case mg of
      Just (GeneralGame g) ->
          let go g gio = do
              (post, g') <- modifyMVar state $ \s -> do
                  (post, g') <- runGameIO gio (c, s) g
                  return (s & cgame c .~ Just (GeneralGame g'), (post, g'))
              case post of
                Just Delayed{..} -> void . forkIO $ do
                    threadDelay delay
                    go g' act
                _ -> return ()
           in go g (fromMaybe (return Done) $ recvT msg)
      Nothing -> return ()

runUserlist :: Client -> ServerState -> IO ServerState
runUserlist c s = case s^.cgame c of
                    Just (GeneralGame g) -> runGameIO userlist (c, s) g $> s
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
