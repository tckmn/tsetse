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

module OCWall where

import Data.Text (Text)
import GHC.Generics

import Data.Aeson

import Types
import Templates

data OCWallGame = OCWallGame { wall :: [(Int,Text)]
                             , groups :: [Int]
                             , strikes :: Int
                             , startTime :: Integer
                             , duration :: Int
                             }

$(makeSetters ''OCWallGame)

data OCWallMsg = OCWallSetWall { categories :: [Text] }
               | OCWallGuess { guess :: [Int] }
               deriving Generic

instance FromJSON OCWallMsg where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON OCWallMsg where
    toJSON = genericToJSON jsonOpts
    toEncoding = genericToEncoding jsonOpts

instance Game OCWallGame OCWallMsg where

    recv c OCWallSetWall{..} = do
        return ()

    recv c OCWallGuess{..} = do
        setStrikes 10
        return ()
