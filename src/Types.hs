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
import Data.List (nub)
import Data.Text (Text)
import qualified Data.HashMap.Strict as M
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

-- main game type
class FromJSON msg => Game g msg | g -> msg where
    new :: IO g
    catchup :: GameIO g ()
    players :: g -> [ClientId]
    userinfo :: g -> ClientId -> Value
    desc :: g -> Text
    recv :: msg -> GameIO g ()

    recvT :: Text -> Maybe (GameIO g ())
    recvT t = recv <$> decodeT t

    -- TODO figure out how to use broadcast/lenses here
    userlist :: GameIO g ()
    userlist = do
        info <- gets userinfo
        ugid <- view $ _1.gid
        ServerState{..} <- view _2
        let clients = _clients^..folded.filteredBy (gid.only ugid)
        let cids = clients^..folded.cid
        pids <- gets players
        liftIO . broadcastWS clients $ object
            [ ("t", String "UserList")
            , ("list", toJSON $ fix _users cids pids .$. info <$> nub (cids ++ pids))
            ]
            where fix users cids pids uid (Object o) = Object
                    . M.delete "t"
                    . M.insert "uid" (Number $ fromIntegral uid)
                    . M.insert "name" (String $ users^.folded.filtered ((==uid) . _uid).uname)
                    . M.insert "conn" (Bool $ uid `elem` cids)
                    . M.insert "play" (Bool $ uid `elem` pids)
                    $ o
                  fix _ _ _ _ x = x

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
                               , _admins :: [ClientId]
                               , _nextConn :: Int
                               , _nextClient :: ClientId
                               , _nextGame :: GameId
                               , _password :: Text
                               , _games :: M.HashMap Int GeneralGame
                               }
makeLenses ''ServerState

byUid :: ClientId -> Fold ServerState User
byUid u = users.folded.filteredBy (uid.only u)

cgame :: Client -> Lens' ServerState (Maybe GeneralGame)
cgame c = games.at (c^.gid)

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
