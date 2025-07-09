{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module AllGames.Asset (AssetGame, AssetCard, SVConf(AssetConf)) where

import AllGames.SetVariant
import Data.List (findIndex, permutations)
import GHC.Generics
import Types
import Util

newtype Card = Card [Int] deriving (Eq, Generic, Show)
instance Torsor Card where
    type GroupElement Card = [Maybe Int]
    Card a @- Card b = [findIndex (==x) b | x <- a]
makeCard ''Card

instance SetVariant Card where
    data SVConf Card = AssetConf { conf :: TorsorConf } deriving Generic
    name _ = "A5SET"
    setSizes = torsorSizes . conf
    fullDeck _ = Card <$> (filter evenP $ permutations [0..4])
        where evenP a = even $ length [0 | i <- [0..4], j <- [i..4], a !! i > a !! j]
    checkSet = torsor . conf

type AssetGame = SetVariantGame Card
type AssetCard = Card
