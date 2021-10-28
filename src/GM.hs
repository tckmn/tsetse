{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module GM where

import GHC.Generics
import Types

data GMMsg = Register { i_uname :: Text }
           | Identify { i_cid :: ClientId, i_secret :: Text }
           | Uname { i_uname :: Text }
           | JoinGame { i_gid :: GameId }
           | CreateGame { i_gtype :: Text }
           deriving Generic
makeJSON ''GMMsg

data GMOutMsg = Registered { o_cid :: ClientId, o_secret :: Text, o_name :: Text }
              | Identified { o_name :: Text }
              | NotIdentified
              | GameList { o_list :: [(GameId, (Text, Text, Text, UTCTime))] }
              | GameType { o_gtype :: Text }
              | Highlight { o_idxs :: [Int], o_good :: Bool }
              | Toast { o_msg :: Text }
              deriving Generic
makeJSON ''GMOutMsg
