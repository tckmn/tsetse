{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

module Types where

import Control.Monad
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LB

import Data.Aeson
import qualified Network.WebSockets as WS

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.State
type GameIO' g = ReaderT ServerState (StateT g (MaybeT IO))

type ClientId = Text
data Connection = Connection WS.Connection Int
instance Eq Connection where
    (Connection _ n1) == (Connection _ n2) = n1 == n2

data Client = Client ClientId Connection
clientId   (Client cid _)  = cid
clientConn (Client _ conn) = conn
withCid cid = filter ((==cid) . clientId)

-- what an absolute beast of a monad
newtype GameIO g a = GameIO { runGameIO :: ServerState -> g -> IO (Maybe a, g) }

instance Monad (GameIO g) where
    return x = GameIO $ \s g -> return (Just x, g)
    (GameIO h) >>= f = GameIO $ \s g -> do
        (a, g') <- h s g
        case a of
          Nothing -> return (Nothing, g')
          Just a -> let GameIO h' = f a in h' s g'

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
