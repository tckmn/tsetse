{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Cset where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import GHC.Generics
import qualified Data.HashMap.Strict as M

import GameUtil
import Types
import Util

import Data.Aeson

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
                         , _scores :: M.HashMap ClientId Int
                         }
makeLenses ''CsetGame

data Msg = Claim { idxs :: [Int] }
         deriving Generic
makeJSON ''Msg

data OutMsg = Highlight { o_idxs :: [Int], o_good :: Bool }
            | Cards { o_cards :: [Card] }
            | UserInfo { o_score :: Int }
            deriving Generic
makeJSON ''OutMsg

instance Game CsetGame Msg where

    new = do
        shuf <- shuffle fullDeck
        let (cards, deck) = splitAt 12 fullDeck
        return CsetGame { _deck = deck
                        , _cards = cards
                        , _scores = M.empty
                        }

    catchup = do
        cs <- use cards
        send $ Cards cs

    players g = g^.scores&M.keys

    userinfo g cid = toJSON $ UserInfo (g^.scores.at cid.non 0)

    desc g = "c53t"

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

        -- gain some score
        who <- view $ _1.cid
        scores.at who %= Just . maybe 1 succ
        userlist

        -- oh my god what a beautiful line
        newCards <- deck %%= splitAt 5
        cs <- cards <%= (itraversed %@~ \i c -> fromMaybe c . lookup i $ zip idxs' newCards)
        broadcast $ Cards cs
