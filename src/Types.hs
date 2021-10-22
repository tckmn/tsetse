{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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
import Data.Functor
import Data.HashMap.Strict as HM
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T

import Control.Lens
import Data.Aeson
import System.Random
import qualified Network.WebSockets as WS

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.State

import Util
import Network

import Language.Haskell.TH

-- main game monad
type GameIO g = ReaderT (Client, ServerState) (MaybeT (StateT g IO))
runGameIO :: GameIO g a -> (Client, ServerState) -> g -> IO (Maybe a, g)
runGameIO = ((runStateT . runMaybeT) .) . runReaderT
-- runGameIO' :: GameIO g a -> Client -> ServerState -> IO (Maybe a, g)
-- runGameIO' g c s@ServerState{_game} = runGameIO g (c, s) _game

-- main game type
class FromJSON msg => Game g msg | g -> msg where
    new :: IO g
    catchup :: GameIO g ()
    userlist :: g -> ClientId -> Value
    recv :: msg -> GameIO g ()

    recvT :: Text -> Maybe (GameIO g ())
    recvT t = recv <$> decodeT t

-- main user type
data User = User { _uid :: ClientId
                 , _secret :: Text
                 , _uname :: Text
                 }
-- TODO why doesn't this work???
-- makeLenses ''User
uid :: Lens' User ClientId
uid = lens _uid (\x y -> x { _uid = y })
secret :: Lens' User Text
secret = lens _secret (\x y -> x { _secret = y })
uname :: Lens' User Text
uname = lens _uname (\x y -> x { _uname = y })

-- main server type
data GeneralGame = forall g msg. Game g msg => GeneralGame g
data ServerState = ServerState { _clients :: [Client]
                               , _users :: [User]
                               , _players :: [ClientId]
                               , _admins :: [ClientId]
                               , _nextConn :: Int
                               , _nextClient :: ClientId
                               , _password :: Text
                               , _game :: GeneralGame
                               }
makeLenses ''ServerState
byUid :: ClientId -> Fold ServerState User
byUid uid = users.folded.filtered ((==uid) . _uid)

-- functions for accessing games without pain
runCatchup :: Client -> ServerState -> IO ()
runCatchup c (s@ServerState{_game=GeneralGame g}) = runGameIO catchup (c, s) g $> ()

runRecv :: Client -> ServerState -> Text -> IO ServerState
runRecv c (s@ServerState{_game=GeneralGame g}) msg = do
    (_, game') <- runGameIO (sequence_ $ recvT msg) (c, s) g
    return $ s & game .~ GeneralGame game'

runUserlist :: Client -> ServerState -> IO ServerState
runUserlist c (s@ServerState{_game=GeneralGame g}) = do
    broadcastWS (s^.clients) $ object
        [ ("t", String "UserList")
        , ("list", toJSON $ fix .$. userlist g <$> s^..clients.traversed.cid)
        ]
    return s
        where fix cid (Object o) =
                Object .
                HM.delete "t" .
                HM.insert "uid" (Number $ fromIntegral cid) .
                HM.insert "name" (String . fromMaybe "???" $ s^?byUid cid.uname) $ o
              fix _ x = x

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

liftWS :: ToJSON a => (t -> a -> IO ()) -> Getting t (Client, ServerState) t -> a -> GameIO g ()
liftWS fn lens msg = view lens >>= \x -> liftIO $ fn x msg

send :: ToJSON a => a -> GameIO g ()
send = liftWS sendWS (_1.conn)

broadcast :: ToJSON a => a -> GameIO g ()
broadcast = liftWS broadcastWS (_2.clients)
