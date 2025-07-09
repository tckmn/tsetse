{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module AllGames.Set (SetGame, SetCard, SVConf(SetConf)) where

import AllGames.SetVariant
import GHC.Generics
import Types
import Util

newtype Card = Card (Int, Int, Int, Int) deriving (Eq, Generic, Show)
instance Semigroup Card where
    Card (a,b,c,d) <> Card (a',b',c',d') = Card ((a+a') `mod` 3, (b+b') `mod` 3, (c+c') `mod` 3, (d+d') `mod` 3)
instance Monoid Card where
    mempty = Card (0,0,0,0)
makeCard ''Card

instance SetVariant Card where
    data SVConf Card = SetConf deriving Generic
    name _ = "SET"
    setSizes _ = [3]
    fullDeck _ = [Card (i,j,k,l) | let r = [0..2], i <- r, j <- r, k <- r, l <- r]
    checkSet _ = (length .==. pure 3) .&&. (mconcat .==. pure mempty)

type SetGame = SetVariantGame Card
type SetCard = Card
