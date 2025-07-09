{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module AllGames.Fold (FoldGame, FoldCard, SVConf(FoldConf)) where

import AllGames.SetVariant
import GHC.Generics
import Types
import Util

data Card = Card (Int, Bool) (Int, Bool) deriving (Eq, Generic, Show)
makeCard ''Card

instance SetVariant Card where
    data SVConf Card = FoldConf deriving Generic
    name _ = "FOLD"
    setSizes _ = [3,5]
    fullDeck _ = [Card (i,a) (j,b) | i <- [1..5], j <- [1..5],
                                   a <- [True,False], b <- [True,False],
                                   not $ i == 3 && j == 3 && a == b]
    checkSet _ set = any (\c -> folds 6 (c:v1) && folds 6 (c:v2)) [(0,True), (0,False)]
        where v1 = [c | Card c _ <- set]
              v2 = [c | Card _ c <- set]

type FoldGame = SetVariantGame Card
type FoldCard = Card
