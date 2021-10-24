{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module OCWall where

import GHC.Generics
import Types

import Data.Aeson

data OCWallGame = OCWallGame { _wall :: [(Int,Text)]
                             , _groups :: [Int]
                             , _strikes :: Int
                             , _startTime :: Integer
                             , _duration :: Int
                             }
makeLenses ''OCWallGame

data Msg = SetWall { i_categories :: [Text] }
         | Guess { i_guess :: [Int] }
         deriving Generic
makeJSON ''Msg

data OutMsg = UserInfo { o_foo :: Int }
            deriving Generic
makeJSON ''OutMsg

instance Game OCWallGame Msg where

    new = return OCWallGame { _wall = []
                            , _groups = []
                            , _strikes = 3
                            , _startTime = 0
                            , _duration = 180
                            }

    catchup = return ()
            -- when (cid `elem` admins)  $ sendWS conn $ encodeAdmin True
            -- when (cid `elem` players) $ sendWS conn $ encodePlayer True
            -- when (not $ null wall)    $ sendWS conn $ encodeWall startTime duration wall
            -- when (not $ null groups)  $ sendWS conn $ encodeGuess True groups
            -- when (strikes /= 3)       $ sendWS conn $ encodeStrikes strikes

    players g = []

    userinfo g cid = toJSON $ UserInfo 10

    desc g = "connecting wall"

    recv SetWall{..} = do
        return Done

    recv Guess{..} = do
        return Done
