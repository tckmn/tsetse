{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Octa (OctaGame, OctaCard) where

import Data.List (findIndex, permutations)
import GHC.Generics
import SetVariant
import Types
import Util

data Card = Card [Int] Bool deriving (Eq, Generic, Show)
instance Diffable Card where
    diff = diffable (@-)
        where Card a b @- Card a' b' = ([findIndex (==x) a | x <- a'], b /= b')
instance Binary Card
makeJSON ''Card

instance SetVariant Card where
    name _ = "OCTA"
    boardSize _ = 9
    setSizes _ = [3]
    fullDeck = [Card a b | a <- permutations [0..3], b <- [False,True]]
    checkSet = linear

type OctaGame = SetVariantGame Card
type OctaCard = Card
