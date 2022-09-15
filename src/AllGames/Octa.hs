{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module AllGames.Octa (OctaGame, OctaCard) where

import AllGames.SetVariant
import Data.List (findIndex, permutations)
import GHC.Generics
import Types
import Util

data Card = Card [Int] Bool deriving (Eq, Generic, Show)
instance Diffable Card where
    type DiffResult Card = ([Maybe Int], Bool)
    Card a b @- Card a' b' = ([findIndex (==x) a | x <- a'], b /= b')
instance Binary Card
makeJSON ''Card

instance SetVariant Card where
    type SVConf Card = NoConf
    name _ = "OCTA"
    setSizes _ = [3]
    fullDeck _ = [Card a b | a <- permutations [0..3], b <- [False,True]]
    checkSet _ = linear

type OctaGame = SetVariantGame Card
type OctaCard = Card
