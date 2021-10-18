{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Cset where

import GHC.Generics
import Types
import Util

newtype Card = Card (Int, Int, Int) deriving Eq
instance Semigroup Card where
    Card (a,b,c) <> Card (a',b',c') = Card (a+a' `mod` 5, b+b' `mod` 5, c+c' `mod` 5)
instance Monoid Card where
    mempty = Card (0,0,0)

fullDeck = [Card (i,j,k) | let r = [0,1,2,3,4], i <- r, j <- r, k <- r]

checkSet :: [Card] -> Bool
checkSet cards = length cards == 5 && mconcat cards == mempty

data CsetGame = CsetGame { _deck :: [Card]
                         , _cards :: [Card]
                         }
makeLenses ''CsetGame

data Msg = Foo { asdfasdf :: Int }
         | Bar { jkljkl :: Text }
         deriving Generic
makeJSON ''Msg

instance Game CsetGame Msg where

    new g = let (shuf, g') = shuffle fullDeck g
                (cards, deck) = splitAt 12 shuf
             in (CsetGame { _deck = deck
                          , _cards = cards
                          }, g)

    recv c msg = return ()
