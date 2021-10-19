{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Cset where

import Control.Concurrent (threadDelay)
import Control.Applicative
import Data.List (nub)
import Data.Maybe (fromMaybe)
import GHC.Generics
import Types
import Util

newtype Card = Card (Int, Int, Int) deriving (Eq, Generic)
instance Semigroup Card where
    Card (a,b,c) <> Card (a',b',c') = Card (a+a' `mod` 5, b+b' `mod` 5, c+c' `mod` 5)
instance Monoid Card where
    mempty = Card (0,0,0)
makeJSON ''Card

fullDeck = [Card (i,j,k) | let r = [0,1,2,3,4], i <- r, j <- r, k <- r]

checkSet :: [Card] -> Bool
checkSet cards = length cards == 5 && length (nub cards) == 5 && mconcat cards == mempty

data CsetGame = CsetGame { _deck :: [Card]
                         , _cards :: [Card]
                         }
makeLenses ''CsetGame

data Msg = Claim { set :: [Card] }
         deriving Generic
makeJSON ''Msg

data OutMsg = Highlight { o_set :: [Card], o_good :: Bool }
            | NewCards { o_cards :: [Card] }
            deriving Generic
makeJSON ''OutMsg

instance Game CsetGame Msg where

    new g = let (shuf, g') = shuffle fullDeck g
                (cards, deck) = splitAt 12 shuf
             in (CsetGame { _deck = deck
                          , _cards = cards
                          }, g)

    recv Claim{..} = do
        -- you can't claim a set that's not on the board
        cs <- use cards
        guard $ all (`elem` cs) set

        -- flash em red if they're not a set
        unless (checkSet set) $ do
            send $ Highlight set False
            empty

        -- they're a set! tell everyone
        broadcast $ Highlight set True
        liftIO $ threadDelay 5000000

        -- oh my god what a beautiful line
        newCards <- deck %%= splitAt 5
        cs <- cards <%= map (\c -> fromMaybe c . lookup c $ zip set newCards)
        broadcast $ NewCards cs
