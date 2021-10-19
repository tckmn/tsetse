{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Types
    ( module Control.Lens
    , module Control.Monad.Reader
    , module Control.Monad.State
    , module System.Random
    , module Network
    , Text
    , module Types
    ) where

import Control.Monad
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Control.Lens
import Data.Aeson
import System.Random
import qualified Network.WebSockets as WS

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.State

import Network

import Language.Haskell.TH

-- main game monad
type GameIO g = ReaderT (Client, ServerState) (MaybeT (StateT g IO))
runGameIO :: GameIO g a -> (Client, ServerState) -> g -> IO (Maybe a, g)
runGameIO = ((runStateT . runMaybeT) .) . runReaderT

-- main game type
class FromJSON msg => Game g msg | g -> msg where
    new :: StdGen -> (g, StdGen)
    recv :: msg -> GameIO g ()
    recvT :: Text -> Maybe (GameIO g ())
    recvT t = recv <$> decode (LB.fromStrict $ T.encodeUtf8 t)

-- main server type
data ServerState = forall g msg. Game g msg =>
    ServerState { _clients :: [Client]
                , _secrets :: Map ClientId Text
                , _players :: [ClientId]
                , _admins :: [ClientId]
                , _nextConn :: Int
                , _password :: Text
                , _game :: g
                }
makeLenses ''ServerState

-- jsonifying message types
killPrefix :: String -> String -> String
killPrefix o "" = o
killPrefix o ('_':s) = s
killPrefix o (_:s) = killPrefix o s

jsonOpts = defaultOptions { sumEncoding = TaggedObject "t" ""
                          , fieldLabelModifier = join killPrefix
                          }

makeJSON :: Name -> DecsQ
makeJSON t = [d|
    instance FromJSON $(pure $ ConT t) where
        parseJSON = genericParseJSON jsonOpts
    instance ToJSON $(pure $ ConT t) where
        toJSON = genericToJSON jsonOpts
        toEncoding = genericToEncoding jsonOpts
    |]

-- common game monad tasks

liftWS :: ToJSON a => (t -> Text -> IO ()) -> Getting t (Client, ServerState) t -> a -> GameIO g ()
liftWS fn lens msg = view lens >>= \x -> liftIO . fn x . lb2t $ encode msg
    where lb2t = T.decodeUtf8 . B.concat . LB.toChunks

send :: ToJSON a => a -> GameIO g ()
send = liftWS sendWS (_1.conn)

broadcast :: ToJSON a => a -> GameIO g ()
broadcast = liftWS broadcastWS (_2.clients)
