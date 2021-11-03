{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Asset (AssetGame) where

import Data.List (findIndex, permutations)
import GHC.Generics
import SetVariant
import Types

newtype Card = Card [Int] deriving (Eq, Generic, Show)
(@-) :: Card -> Card -> [Maybe Int]
Card a @- Card b = [findIndex (==x) b | x <- a]
instance Binary Card
makeJSON ''Card

instance SetVariant Card where
    name _ = "A5SET"
    boardSize _ = 10
    setSizes _ = [3]
    fullDeck = Card <$> (filter evenP $ permutations [0..4])
        where evenP a = even $ length [0 | i <- [0..4], j <- [i..4], a !! i > a !! j]
    checkSet set = any checkSet' $ permutations set
        where checkSet' [a,b,c] = (a @- b) == (b @- c)
              checkSet' _ = False

type AssetGame = SetVariantGame Card
