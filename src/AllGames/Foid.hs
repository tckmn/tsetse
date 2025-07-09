{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module AllGames.Foid (FoidGame, FoidCard, SVConf(FoidConf)) where

import AllGames.SetVariant
import GHC.Generics
import Types
import Util

newtype Card = Card [(Int, Bool)] deriving (Eq, Generic, Show)
makeCard ''Card

instance SetVariant Card where
    data SVConf Card = FoidConf deriving Generic
    name _ = "FO1D"
    setSizes _ = [3,5]
    fullDeck _ = [Card [(i,b),((i+1)`mod`10,b)] | i <- [0..9], b <- [True,False]]
            ++ [Card [(i,b),((i+3)`mod`10,c)] | i <- [0..9], b <- [True,False], c <- [True,False]]
            ++ [Card [(i,b),((i+5)`mod`10,not b)] | i <- [0..4], b <- [True,False]]
    checkSet _ set = folds 10 [c | Card pair <- set, c <- pair]

type FoidGame = SetVariantGame Card
type FoidCard = Card
