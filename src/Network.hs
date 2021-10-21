{-# LANGUAGE TemplateHaskell #-}

module Network where

import Prelude hiding (log)

import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)

import Control.Monad
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T

import Util
import Data.Aeson
import Control.Lens
import qualified Network.WebSockets as WS

-- clients
type ClientId = Int
data Connection = Connection WS.Connection Int
instance Eq Connection where
    (Connection _ n1) == (Connection _ n2) = n1 == n2

data Client = Client { _cid :: ClientId, _conn :: Connection }
makeLenses ''Client
withCid cid = filter ((==cid) . _cid)

-- logging
log :: String -> IO ()
log s = getZonedTime >>= putStrLn . (++s) . formatTime defaultTimeLocale "[%F %T] "

logC :: Connection -> String -> IO ()
logC (Connection _ connid) s = log $ pad connid ++ ": " ++ s
    where pad = reverse . take 5 . (++repeat '0') . reverse . show

-- sending and receiving websocket messages
sendWS :: ToJSON a => Connection -> a -> IO ()
sendWS c@(Connection conn connid) msg = do
    let t = encodeT msg
    logC c $ "SEND " ++ T.unpack t
    WS.sendTextData conn t

recvWS :: Connection -> IO Text
recvWS c@(Connection conn connid) = do
    t <- WS.receiveData conn
    logC c $ "RECV " ++ T.unpack t
    return t

broadcastWS :: ToJSON a => [Client] -> a -> IO ()
broadcastWS clients msg = forM_ clients $ flip sendWS msg . _conn
