{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Cset (CsetGame) where

import GHC.Generics
import SetVariant
import Types
import Util

newtype Card = Card (Int, Int, Int) deriving (Eq, Generic, Show)
instance Semigroup Card where
    Card (a,b,c) <> Card (a',b',c') = Card ((a+a') `mod` 5, (b+b') `mod` 5, (c+c') `mod` 5)
instance Monoid Card where
    mempty = Card (0,0,0)
instance Binary Card
makeJSON ''Card

instance SetVariant Card where
    name _ = "C53T"
    boardSize _ = 12
    setSizes _ = [5]
    fullDeck = [Card (i,j,k) | let r = [0..4], i <- r, j <- r, k <- r]
    checkSet = (length .==. pure 5) .&&. (mconcat .==. pure mempty)

type CsetGame = SetVariantGame Card
