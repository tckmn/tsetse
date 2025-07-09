{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module AllGames.Set2 (SectGame, SectCard) where

import AllGames.SetVariant
import Control.Monad
import Data.List (findIndex, permutations)
import GHC.Generics
import Types
import Util

newtype Card = Card (Int, Int, Int, Int, Int, Int) deriving (Eq, Generic, Show)
instance Torsor Card where
    type GroupElement Card = [Maybe Int]
    Card (a,b,c,d,e,f) @- Card (a',b',c',d',e',f') = [findIndex (==x) arr' | x <- arr]
        where arr = [a,b,c,d,e,f]
              arr' = [a',b',c',d',e',f']
makeCard ''Card

instance SetVariant Card where
    data SVConf Card = Conf { conf :: TorsorConf } deriving Generic
    name _ = "S3CT"
    setSizes = torsorSizes . conf
    fullDeck _ = join [[Card (a,b,c,x,y,z), Card (x,y,z,a,b,c)]
      | [a,b,c] <- permutations [0,1,2], [x,y,z] <- permutations [3,4,5]]
    checkSet = torsor . conf

type SectGame = SetVariantGame Card
type SectCard = Card
