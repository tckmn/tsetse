{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Set2 (Set2Game) where

import Data.List (findIndex, permutations)
import GHC.Generics
import SetVariant
import Types

newtype Card = Card (Int, Int, Int, Int, Int, Int) deriving (Eq, Generic, Show)
(@-) :: Card -> Card -> [Maybe Int]
Card (a,b,c,d,e,f) @- Card (a',b',c',d',e',f') = [findIndex (==x) arr' | x <- arr]
    where arr = [a,b,c,d,e,f]
          arr' = [a',b',c',d',e',f']
makeJSON ''Card

instance SetVariant Card where
    name _ = "S3T2"
    boardSize _ = 12
    setSizes _ = [3]
    fullDeck = join [[Card (a,b,c,x,y,z), Card (x,y,z,a,b,c)]
      | [a,b,c] <- permutations [0,1,2], [x,y,z] <- permutations [3,4,5]]
    checkSet set = any checkSet' $ permutations set
        where checkSet' [a,b,c] = (a @- b) == (b @- c)
              checkSet' _ = False

type Set2Game = SetVariantGame Card
