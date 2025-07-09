{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module AllGames.Pro (ProGame, ProCard, SVConf(ProConf)) where

import AllGames.SetVariant
import GHC.Generics
import Types
import Util

newtype Card = Card (Bool, Bool, Bool, Bool, Bool, Bool) deriving (Eq, Generic, Show)
instance Semigroup Card where
    Card (a,b,c,d,e,f) <> Card (a',b',c',d',e',f') = Card (a/=a', b/=b', c/=c', d/=d', e/=e', f/=f')
instance Monoid Card where
    mempty = Card (False, False, False, False, False, False)
makeCard ''Card

instance SetVariant Card where
    data SVConf Card = ProConf deriving Generic
    name _ = "PRO"
    setSizes _ = [2,3,4,5,6,7]
    fullDeck _ = [Card (i,j,k,l,m,n) | let r = [True,False], i<-r, j<-r, k<-r, l<-r, m<-r, n<-r, i||j||k||l||m||n]
    checkSet _ = mconcat .==. pure mempty

type ProGame = SetVariantGame Card
type ProCard = Card
