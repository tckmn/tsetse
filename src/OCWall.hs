{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module OCWall where

import Data.Text (Text)
import GHC.Generics

import Control.Lens
import Data.Aeson hiding ((.=))

import Types
import Templates
import ServerTemplates

import Control.Monad.Reader
import Control.Monad.State

data OCWallGame = OCWallGame { _wall :: [(Int,Text)]
                             , _groups :: [Int]
                             , _strikes :: Int
                             , _startTime :: Integer
                             , _duration :: Int
                             }
makeLenses ''OCWallGame


-- $(makeGameFns ''OCWallGame)
-- $(makeGameFns' ''OCWallGame)

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
        -- let x = modStrikes pred :: GameIO' OCWallGame Int
        s <- use strikes
        s' <- gets . view $ strikes
        strikes += 1
        strikes %= succ
        strikes .= 2
        cs <- flip magnify ask clients
        cs' <- asks . view $ clients
        cs'' <- view clients
        return ()
