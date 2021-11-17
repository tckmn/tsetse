{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Sat (SatGame, SatCard) where

import Data.List (sort, nub)
import GHC.Generics
import SetVariant
import Types
import Util

nvars = 8
nclause = 4

newtype Card = Card [Int] deriving (Eq, Generic, Show)
instance Binary Card
makeJSON ''Card

-- sat :: [Card] -> Bool
-- sat constraints = any (allOf constraints . sats) . sequence . take nvars $ repeat [True,False]
--     where allOf = flip all
--           sats vars (Card constraint) = any (good vars) constraint
--           good vars n
--             | n > 0     = vars !! (n-1)
--             | otherwise = not $ vars !! (-(n+1))

sat :: [Card] -> Bool
sat constraints = any (allOf constraints . sats) . sequence . take nvars $ repeat [True,False]
    where allOf = flip all
          sats vars (Card constraint) = (length $ filter (good vars) constraint) == 1
          good vars n
            | n > 0     = vars !! (n-1)
            | otherwise = not $ vars !! (-(n+1))

instance SetVariant Card where
    type SVConf Card = NoConf
    name _ = "SAT"
    boardSize _ = 8
    setSizes _ = []
    fullDeck = map Card . nub . map sort . filter (\x -> length (nub x) == nclause) . sequence . take nclause $ repeat [1..nvars]
    -- fullDeck = [Card [a*as,b*bs,c*cs] | a <- [1..nvars], b <- [a+1..nvars], c <- [b+1..nvars]
    --                                   , as <- [(-1),1], bs <- [(-1),1], cs <- [(-1),1]
    --                                   ]
    checkSet set = not (sat set) && all (sat . flip deleteAt set) [0..length set-1]
        where deleteAt 0 (x:xs) = xs
              deleteAt i (x:xs) = x:deleteAt (i-1) xs
              deleteAt _ [] = []
    noSets (_, cs) = sat cs

type SatGame = SetVariantGame Card
type SatCard = Card
