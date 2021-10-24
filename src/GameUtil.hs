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

liftWS :: ToJSON a => (t -> a -> IO ()) -> Getting t (Client, ServerState) t -> a -> GameIO g ()
liftWS fn lens msg = view lens >>= \x -> liftIO $ fn x msg

send :: ToJSON a => a -> GameIO g ()
send = liftWS sendWS (_1.conn)

broadcast :: ToJSON a => a -> GameIO g ()
broadcast = liftWS broadcastWS (_2.clients)
