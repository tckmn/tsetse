{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Cset where

import Data.Text (Text)
import GHC.Generics

import Data.Aeson

import Types
import Templates

data CsetGame = CsetGame { cards :: [Int]
                         }

data SetMsg = SetMsg { asdfasdf :: Int }
    deriving Generic

instance FromJSON SetMsg where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON SetMsg where
    toJSON = genericToJSON jsonOpts
    toEncoding = genericToEncoding jsonOpts


instance Game CsetGame SetMsg where
    recv c msg = return ()

