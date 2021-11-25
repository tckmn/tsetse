{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Network where

import Prelude hiding (log, truncate)

import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Util
import Data.Aeson
import Control.Lens
import qualified Network.WebSockets as WS

-- clients
type ClientId = Int
type GameId = Int
data Connection = Connection WS.Connection Int
instance Eq Connection where
    (Connection _ n1) == (Connection _ n2) = n1 == n2

data Client = Client { _conn :: Connection
                     , _cid :: ClientId
                     , _gid :: GameId
                     }
makeLenses ''Client

-- convenience: can use a client where a connection is desired
class HasConn a where
    getconn :: a -> Connection
instance HasConn Connection where
    getconn = id
instance HasConn Client where
    getconn = view conn

-- logging
log :: Text -> IO ()
log s = getZonedTime >>= T.putStrLn . (<>s) . T.pack . formatTime defaultTimeLocale "[%F %T] "

logC :: HasConn a => a -> Text -> IO ()
logC c s = log $ pad connid <> ": " <> s
    where Connection _ connid = getconn c
          pad = T.pack . reverse . take 5 . (++repeat '0') . reverse . show

-- sending and receiving websocket messages
truncate :: Text -> Text
truncate t = if T.length t > 1000 then T.take 997 t <> "..." else t

sendWS :: (HasConn a, ToJSON b) => a -> b -> IO ()
sendWS hc msg = do
    let c@(Connection conn _) = getconn hc
    let t = encodeT msg
    logC c $ "SEND " <> truncate t
    WS.sendTextData conn t

recvWS :: HasConn a => a -> IO Text
recvWS hc = do
    let c@(Connection conn _) = getconn hc
    t <- WS.receiveData conn
    logC c $ "RECV " <> truncate t
    return t

broadcastWS :: ToJSON a => [Client] -> a -> IO ()
broadcastWS clients msg = forM_ clients $ flip sendWS msg . _conn
