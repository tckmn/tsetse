{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module OCWall where

import Data.Text (Text)
import GHC.Generics

import Data.Aeson

import Types
import Templates
import ServerTemplates

data OCWallGame = OCWallGame { wall :: [(Int,Text)]
                             , groups :: [Int]
                             , strikes :: Int
                             , startTime :: Integer
                             , duration :: Int
                             }

$(makeGameFns ''OCWallGame)

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
        modStrikes pred
        return ()
