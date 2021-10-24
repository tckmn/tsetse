module GameUtil where

import Data.Aeson
import Data.Functor
import GM
import Types
import qualified Data.HashMap.Strict as M

-- functions for accessing games without pain

runCatchup :: Client -> ServerState -> IO ()
runCatchup c s = case s^.cgame c of
                   Just (GeneralGame g) -> runGameIO catchup (c, s) g $> ()
                   Nothing -> sendWS (c^.conn) . GameList $
                       (_2 %~ runDesc) <$> M.toList (s^.games)
    where runDesc (GeneralGame g) = desc g

runRecv :: Client -> ServerState -> Text -> IO ServerState
runRecv c s msg = case s^.cgame c of
                    Just (GeneralGame g) -> do
                        (_, g') <- runGameIO (sequence_ $ recvT msg) (c, s) g
                        return $ s & cgame c .~ Just (GeneralGame g')
                    Nothing -> return s

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
    c <- view $ _2.clients
    liftIO $ broadcastWS (c^..folded.filteredBy (gid.only g)) msg
