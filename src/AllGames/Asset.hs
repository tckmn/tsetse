{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module AllGames.Asset (AssetGame, AssetCard) where

import AllGames.SetVariant
import Data.List (findIndex, permutations)
import GHC.Generics
import Types
import Util

newtype Card = Card [Int] deriving (Eq, Generic, Show)
instance Diffable Card where
    type DiffResult Card = [Maybe Int]
    Card a @- Card b = [findIndex (==x) b | x <- a]
instance Binary Card
makeJSON ''Card

instance SetVariant Card where
    type SVConf Card = NoConf
    name _ = "A5SET"
    setSizes _ = [3]
    fullDeck _ = Card <$> (filter evenP $ permutations [0..4])
        where evenP a = even $ length [0 | i <- [0..4], j <- [i..4], a !! i > a !! j]
    checkSet _ = linear

type AssetGame = SetVariantGame Card
type AssetCard = Card
