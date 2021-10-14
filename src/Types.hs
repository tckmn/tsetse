{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar, modifyMVar_)
import Control.Exception (catch, finally, IOException)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Char (isUpper, isAscii, isSpace, isDigit)
import Data.Functor
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)
import GHC.Generics
import System.Random (randomRIO)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LB

import Data.Aeson
import qualified Network.WebSockets as WS

-- import Control.Monad.Trans
-- import Control.Monad.Reader
-- import Control.Monad.State
-- import Control.Monad.Except

type ClientId = Text
data Connection = Connection WS.Connection Int
instance Eq Connection where
    (Connection _ n1) == (Connection _ n2) = n1 == n2

data Client = Client ClientId Connection
clientId   (Client cid _)  = cid
clientConn (Client _ conn) = conn
withCid cid = filter ((==cid) . clientId)

-- what an absolute beast of a monad
newtype GameIO g a = GameIO { runGameIO :: ServerState -> g -> IO (Either String a, g) }
-- type GameIO' g = ReaderT ServerState (StateT g (ExceptT String IO)) ???

-- instance Applicative (GameIO g) where
--     pure x = GameIO $ \s g -> return (x, g)
--     (GameIO f) <*> (GameIO x) = 

instance Monad (GameIO g) where
    return x = GameIO $ \s g -> return (Right x, g)
    (GameIO h) >>= f = GameIO $ \s g -> do
        (a, g') <- h s g
        case a of
          Left e -> return (Left e, g')
          Right a -> let GameIO h' = f a in h' s g'

instance Applicative (GameIO g) where
    pure = return
    (<*>) = ap

instance Functor (GameIO g) where
    fmap = liftM

jsonOpts = defaultOptions { sumEncoding = TaggedObject "t" "" }

class FromJSON msg => Game g msg | g -> msg where
    recv :: Client -> msg -> GameIO g ()
    recvT :: Client -> Text -> Maybe (GameIO g ())
    recvT c t = recv c <$> decode (LB.fromStrict $ T.encodeUtf8 t)


data ServerState = forall g msg. Game g msg =>
    ServerState { clients :: [Client]
                , secrets :: Map ClientId Text
                , players :: [ClientId]
                , admins :: [ClientId]
                , nextConn :: Int
                , password :: Text
                , game :: g
                }
