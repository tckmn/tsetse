{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module GM where

import Data.Aeson
import GHC.Generics
import Types

data GMMsg = Register { i_uname :: Text }
           | Identify { i_cid :: ClientId, i_secret :: Text }
           | Uname { i_uname :: Text }
           | JoinGame { i_gid :: GameId }
           | CreateGame { i_gtype :: Text, i_conf :: Value }
           | ModifyGame { i_gid :: GameId, i_conf :: Value }
           | DeleteGame { i_gid :: GameId, i_mpassword :: Maybe Text }
           | GetScores
           -- admin
           | SaveState { i_password :: Text }
           deriving Generic
makeJSON ''GMMsg

data GMOutMsg = Registered { o_cid :: ClientId, o_secret :: Text, o_name :: Text }
              | NotRegistered
              | Identified { o_name :: Text }
              | NotIdentified
              | GameList { o_list :: [Value] }
              | GameType { o_gtype :: Text, o_conf :: Value }
              | Highlight { o_who :: ClientId, o_idxs :: [Int], o_good :: Bool }
              | Scores { o_scores :: HashMap Text (HashMap Text Int) }
              | Toast { o_msg :: Text }
              deriving Generic
makeJSON ''GMOutMsg
