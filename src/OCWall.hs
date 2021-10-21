{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module OCWall where

import GHC.Generics
import Types

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

instance Game OCWallGame Msg where

    new = return OCWallGame { _wall = []
                            , _groups = []
                            , _strikes = 3
                            , _startTime = 0
                            , _duration = 180
                            }

    catchup = return ()

    recv SetWall{..} = do
        return ()

    recv Guess{..} = do
        s <- use strikes
        strikes += 1
        strikes %= succ
        strikes .= 2
        return ()
