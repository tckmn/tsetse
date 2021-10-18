{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Types
    ( module Control.Lens
    , module Control.Monad.Reader
    , module Control.Monad.State
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
import qualified Data.ByteString.Lazy as LB

import Control.Lens
import Data.Aeson
import qualified Network.WebSockets as WS

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.State

import Language.Haskell.TH

-- clients
type ClientId = Text
data Connection = Connection WS.Connection Int
instance Eq Connection where
    (Connection _ n1) == (Connection _ n2) = n1 == n2

data Client = Client ClientId Connection
clientId   (Client cid _)  = cid
clientConn (Client _ conn) = conn
withCid cid = filter ((==cid) . clientId)

-- main game monad
type GameIO g = ReaderT ServerState (MaybeT (StateT g IO))
runGameIO :: GameIO g a -> ServerState -> g -> IO (Maybe a, g)
runGameIO = ((runStateT . runMaybeT) .) . runReaderT

-- main game type
class FromJSON msg => Game g msg | g -> msg where
    recv :: Client -> msg -> GameIO g ()
    recvT :: Client -> Text -> Maybe (GameIO g ())
    recvT c t = recv c <$> decode (LB.fromStrict $ T.encodeUtf8 t)

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
jsonOpts = defaultOptions { sumEncoding = TaggedObject "t" "" }
makeJSON :: Name -> DecsQ
makeJSON t = [d|
    instance FromJSON $(pure $ ConT t) where
        parseJSON = genericParseJSON jsonOpts
    instance ToJSON $(pure $ ConT t) where
        toJSON = genericToJSON jsonOpts
        toEncoding = genericToEncoding jsonOpts
    |]
