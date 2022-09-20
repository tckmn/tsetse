{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module AllGames.Cece (CeceGame, CeceCard) where

import AllGames.SetVariant
import GHC.Generics
import Types
import Util

data Card = Card Int (Int, Int, Int) deriving (Eq, Generic, Show)
instance Torsor Card where
    type GroupElement Card = (Int, (Int, Int, Int))
    Card n (a,b,c) @- Card n' (a',b',c')
      | d == 0 = (d, ((a-a') `mod` 3, (b-b') `mod` 3, (c-c') `mod` 3))
      | d == 1 = (d, ((a-b') `mod` 3, (b-c') `mod` 3, (c-a') `mod` 3))
      | d == 2 = (d, ((a-c') `mod` 3, (b-a') `mod` 3, (c-b') `mod` 3))
      where d = (n-n') `mod` 3
makeCard ''Card

instance SetVariant Card where
    data SVConf Card = Conf { conf :: TorsorConf } deriving Generic
    name _ = "C3C3"
    setSizes = torsorSizes . conf
    fullDeck _ = [Card n (a,b,c) | n <- [0..2], a <- [0..2], b <- [0..2], c <- [0..2]]
    checkSet = torsor . conf

type CeceGame = SetVariantGame Card
type CeceCard = Card
