{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module AllGames.Cset (CsetGame, CsetCard, SVConf(CsetConf)) where

import AllGames.SetVariant
import GHC.Generics
import Types
import Util

newtype Card = Card (Int, Int, Int) deriving (Eq, Generic, Show)
instance Semigroup Card where
    Card (a,b,c) <> Card (a',b',c') = Card ((a+a') `mod` 5, (b+b') `mod` 5, (c+c') `mod` 5)
instance Monoid Card where
    mempty = Card (0,0,0)
makeCard ''Card

instance SetVariant Card where
    data SVConf Card = CsetConf deriving Generic
    name _ = "C53T"
    setSizes _ = [5]
    fullDeck _ = [Card (i,j,k) | let r = [0..4], i <- r, j <- r, k <- r]
    checkSet _ = (length .==. pure 5) .&&. (mconcat .==. pure mempty)

type CsetGame = SetVariantGame Card
type CsetCard = Card
