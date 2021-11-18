{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module AllGames.Set2 (SectGame, SectCard) where

import AllGames.SetVariant
import Data.List (findIndex, permutations)
import GHC.Generics
import Types

newtype Card = Card (Int, Int, Int, Int, Int, Int) deriving (Eq, Generic, Show)
(@-) :: Card -> Card -> [Maybe Int]
Card (a,b,c,d,e,f) @- Card (a',b',c',d',e',f') = [findIndex (==x) arr' | x <- arr]
    where arr = [a,b,c,d,e,f]
          arr' = [a',b',c',d',e',f']
instance Binary Card
makeJSON ''Card

instance SetVariant Card where
    type SVConf Card = NoConf
    name _ = "S3CT"
    setSizes _ = [3]
    fullDeck = join [[Card (a,b,c,x,y,z), Card (x,y,z,a,b,c)]
      | [a,b,c] <- permutations [0,1,2], [x,y,z] <- permutations [3,4,5]]
    checkSet _ set = any checkSet' $ permutations set
        where checkSet' [a,b,c] = (a @- b) == (b @- c)
              checkSet' _ = False

type SectGame = SetVariantGame Card
type SectCard = Card
