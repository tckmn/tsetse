{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Foid (FoidGame) where

import Data.List (sortOn)
import GHC.Generics
import SetVariant
import Types
import Util

newtype Card = Card [(Int, Bool)] deriving (Eq, Generic, Show)
makeJSON ''Card

notouch :: [(Int, Bool)] -> Bool
notouch ((n1,a1):c@(n2,a2):cs) = n1 /= n2 && notouch (c:cs)
notouch _ = True

foldable :: Int -> [(Int, Bool)] -> Bool
foldable paper [(n1,a1),(n2,a2)] = n2 - n1 == paper `div` 2 && a1 == a2
foldable paper cs = or [ and [ ass i /= ass (i+1)
                             , dist (i-1) >= dist i
                             , dist (i+1) >= dist i
                             , foldable (paper - 2*dist i) (crimp i)
                             ]
                       | i <- [0..length cs - 1]
                       ]
    where num i = fst $ cs !! (i `mod` length cs)
          ass i = snd $ cs !! (i `mod` length cs)
          dist i = (num (i+1) - num i) `mod` paper
          crimp i = [(n',a) | (n,a) <- cs,
                              n /= num i && n /= num (i+1),
                              let n' = if n > num (i+1) then n - 2*dist i else n
                    ]

instance SetVariant Card where
    name _ = "FO1D"
    boardSize _ = 12
    fullDeck = [Card [(i,b),((i+1)`mod`10,b)] | i <- [0..9], b <- [True,False]]
            ++ [Card [(i,b),((i+3)`mod`10,c)] | i <- [0..9], b <- [True,False], c <- [True,False]]
            ++ [Card [(i,b),((i+5)`mod`10,not b)] | i <- [0..4], b <- [True,False]]
    checkSet set = notouch .&&. foldable 10 $ sortOn fst [c | Card pair <- set, c <- pair]

type FoidGame = SetVariantGame Card
