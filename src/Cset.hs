{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Cset where

import GHC.Generics
import Types

data CsetGame = CsetGame { _cards :: [Int]
                         }
makeLenses ''CsetGame

data Msg = Foo { asdfasdf :: Int }
         | Bar { jkljkl :: Text }
         deriving Generic
makeJSON ''Msg

instance Game CsetGame Msg where
    recv c msg = return ()
