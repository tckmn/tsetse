{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Cece (CeceGame, CeceCard) where

import GHC.Generics
import SetVariant
import Types
import Util

data Card = Card Int (Int, Int, Int) deriving (Eq, Generic, Show)
instance Diffable Card where
    diff = diffable (@-)
        where Card n (a,b,c) @- Card n' (a',b',c')
                | d == 0 = (d, ((a-a') `mod` 3, (b-b') `mod` 3, (c-c') `mod` 3))
                | d == 1 = (d, ((a-b') `mod` 3, (b-c') `mod` 3, (c-a') `mod` 3))
                | d == 2 = (d, ((a-c') `mod` 3, (b-a') `mod` 3, (c-b') `mod` 3))
                where d = (n-n') `mod` 3
instance Binary Card
makeJSON ''Card

instance SetVariant Card where
    type SVConf Card = NoConf
    name _ = "C3C3"
    boardSize _ = 12
    setSizes _ = [3]
    fullDeck = [Card n (a,b,c) | n <- [0..2], a <- [0..2], b <- [0..2], c <- [0..2]]
    checkSet = linear

type CeceGame = SetVariantGame Card
type CeceCard = Card
