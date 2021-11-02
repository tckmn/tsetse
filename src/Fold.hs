{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Fold (FoldGame) where

import GHC.Generics
import SetVariant
import Types
import Util

data Card = Card (Int, Bool) (Int, Bool) deriving (Eq, Generic, Show)
makeJSON ''Card

instance SetVariant Card where
    name _ = "FOLD"
    boardSize _ = 12
    fullDeck = [Card (i,a) (j,b) | i <- [1..5], j <- [1..5],
                                   a <- [True,False], b <- [True,False],
                                   not $ i == 3 && j == 3 && a == b]
    checkSet set = any (\c -> folds 6 (c:v1) && folds 6 (c:v2)) [(0,True), (0,False)]
        where v1 = [c | Card c _ <- set]
              v2 = [c | Card _ c <- set]

type FoldGame = SetVariantGame Card
