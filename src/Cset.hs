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

newtype Card = Card (Int, Int, Int) deriving (Eq, Generic, Show)
instance Semigroup Card where
    Card (a,b,c) <> Card (a',b',c') = Card ((a+a') `mod` 5, (b+b') `mod` 5, (c+c') `mod` 5)
instance Monoid Card where
    mempty = Card (0,0,0)
makeJSON ''Card

fullDeck = [Card (i,j,k) | let r = [0,1,2,3,4], i <- r, j <- r, k <- r]

checkSet :: [Card] -> Bool
checkSet = mconcat .==. pure mempty

data CsetGame = CsetGame { _deck :: [Card]
                         , _cards :: [Card]
                         }
makeLenses ''CsetGame

data Msg = Claim { idxs :: [Int] }
         deriving Generic
makeJSON ''Msg

data OutMsg = Highlight { o_idxs :: [Int], o_good :: Bool }
            | Cards { o_cards :: [Card] }
            deriving Generic
makeJSON ''OutMsg

instance Game CsetGame Msg where

    new = do
        shuf <- shuffle fullDeck
        let (cards, deck) = splitAt 12 shuf
        return CsetGame { _deck = deck
                        , _cards = cards
                        }

    catchup = do
        cs <- use cards
        send $ Cards cs

    userlist = do
        return ()

    recv Claim{..} = do
        -- make sure the request is well-formed
        cs <- use cards
        let idxs' = nub idxs
            set = [c | idx <- idxs', let Just c = cs^?ix idx]
        guard $ length set == 5

        -- flash em red if they're not a set
        unless (checkSet set) $ do
            send $ Highlight idxs' False
            empty

        -- they're a set! tell everyone
        broadcast $ Highlight idxs' True
        liftIO $ threadDelay 5000000

        -- oh my god what a beautiful line
        newCards <- deck %%= splitAt 5
        cs <- cards <%= (itraversed %@~ \i c -> fromMaybe c . lookup i $ zip idxs' newCards)
        broadcast $ Cards cs
