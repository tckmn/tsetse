{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module AllGames.Foid (FoidGame, FoidCard) where

import AllGames.SetVariant
import GHC.Generics
import Types
import Util

newtype Card = Card [(Int, Bool)] deriving (Eq, Generic, Show)
instance Binary Card
makeJSON ''Card

instance SetVariant Card where
    type SVConf Card = NoConf
    name _ = "FO1D"
    boardSize _ = 12
    setSizes _ = [3,5]
    fullDeck = [Card [(i,b),((i+1)`mod`10,b)] | i <- [0..9], b <- [True,False]]
            ++ [Card [(i,b),((i+3)`mod`10,c)] | i <- [0..9], b <- [True,False], c <- [True,False]]
            ++ [Card [(i,b),((i+5)`mod`10,not b)] | i <- [0..4], b <- [True,False]]
    checkSet set = folds 10 [c | Card pair <- set, c <- pair]

type FoidGame = SetVariantGame Card
type FoidCard = Card
