{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module GM where

import GHC.Generics
import Types

data Msg = Register { i_uname :: Text }
         | Identify { i_cid :: ClientId, i_secret :: Text }
         | Uname { i_uname :: Text }
         deriving Generic
makeJSON ''Msg

data OutMsg = Registered { o_cid :: ClientId, o_secret :: Text, o_name :: Text }
            | Identified { o_name :: Text }
            | NotIdentified
            deriving Generic
makeJSON ''OutMsg
