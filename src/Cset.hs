{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Cset where

import Data.Text (Text)
import GHC.Generics

import Data.Aeson

import Types
import Templates

data CsetGame = CsetGame { cards :: [Int]
                         }

$(makeMonadFns ''CsetGame)

data SetMsg = SetMsg { asdfasdf :: Int }
    deriving Generic

instance FromJSON SetMsg where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON SetMsg where
    toJSON = genericToJSON jsonOpts
    toEncoding = genericToEncoding jsonOpts

instance Game CsetGame SetMsg where
    recv c msg = return ()
