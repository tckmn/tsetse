{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module AllGames.Octa (OctaGame, OctaCard) where

import AllGames.SetVariant
import Data.List (findIndex, permutations)
import GHC.Generics
import Types
import Util

data Card = Card [Int] Bool deriving (Eq, Generic, Show)
instance Torsor Card where
    type GroupElement Card = ([Maybe Int], Bool)
    Card a b @- Card a' b' = ([findIndex (==x) a | x <- a'], b /= b')
makeCard ''Card

instance SetVariant Card where
    data SVConf Card = Conf { conf :: TorsorConf } deriving Generic
    name _ = "OCTA"
    setSizes = torsorSizes . conf
    fullDeck _ = [Card a b | a <- permutations [0..3], b <- [False,True]]
    checkSet = torsor . conf

type OctaGame = SetVariantGame Card
type OctaCard = Card
