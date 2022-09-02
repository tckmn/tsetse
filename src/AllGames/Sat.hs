{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module AllGames.Sat (SatGame, SatCard, Conf(..), Cond(..), AssignMode(..)) where

import AllGames.SetVariant
import Data.List (sort, nub, subsequences)
import GHC.Generics
import Types
import Util

import Data.Binary as B

big :: Int
big = 100

newtype Card = Card [Int] deriving (Eq, Generic, Show)
instance Binary Card
makeJSON ''Card

data Cond = AtLeast Int | AtMost Int | Exactly Int deriving Generic
instance ToJSON Cond
instance FromJSON Cond
instance Binary Cond

data AssignMode = Exists | FromCards deriving (Generic, Eq)
instance ToJSON AssignMode
instance FromJSON AssignMode
instance Binary AssignMode

data Conf = Conf { nvars :: Int
                 , nclause :: [Int]
                 , hasneg :: Bool
                 , cond :: Cond
                 , assignmode :: AssignMode
                 } deriving Generic
instance ToJSON Conf
instance FromJSON Conf
instance Binary Conf --where
    -- get = do
    --     x <- B.get :: Get NoConf
    --     pure $ Conf 8 [4] False (Exactly 1) Exists

-- is this set of cards satisfiable?
sat :: Conf -> [Card] -> Bool
sat Conf{..} cards = any (allOf constraints . vsats) (pvars assignmode)
    where allOf = flip all
          arrs = (\(Card c) -> c) <$> cards
          constraints = filter (< big) <$> arrs
          -- which possible variable assignments are we considering?
          pvars Exists = sequence . take nvars $ repeat [True,False]
          pvars FromCards = [[any (i+big `elem`) arrs | i <- [1..nvars]]]
          -- does this particular variable assignment satisfy this particular constraint?
          vsats vars constraint = test cond (length constraint) . length $
              filter (good vars) constraint
          -- is this variable true under this assignment?
          good vars n
            | n > 0     = vars !! (n-1)
            | otherwise = not $ vars !! (-(n+1))
          -- is this a satisfying number of true variables?
          test (AtLeast n) m
            | n >= 0    = (>= n)
            | otherwise = (>= m+n+1)
          test (AtMost n) m
            | n >= 0    = (<= n)
            | otherwise = (<= m+n+1)
          test (Exactly n) m
            | n >= 0    = (== n)
            | otherwise = (== m+n+1)

instance SetVariant Card where
    type SVConf Card = Conf
    name _ = "SAT"
    setSizes _ = [2..99]
    fullDeck Conf{..} = Card <$> (base >>= tass >>= tneg)
        where base = filter (flip elem nclause . length) $ subsequences [1..nvars]
              donegs [] = [[]]
              donegs (x:xs) = (if x > big then [] else (-x:) <$> donegs xs) ++ ((x:) <$> donegs xs)
              doass [] = []
              doass (x:xs) = ((x+big):xs):((x:) <$> doass xs)
              tneg
                | hasneg                  = donegs
                | otherwise               = pure
              tass
                | assignmode == FromCards = doass
                | otherwise               = pure
    checkSet conf@Conf{assignmode=FromCards} set = sat conf set
    checkSet conf set = not (sat conf set) && all (sat conf . flip deleteAt set) [0..length set-1]
        where deleteAt 0 (x:xs) = xs
              deleteAt i (x:xs) = x:deleteAt (i-1) xs
              deleteAt _ [] = []
    noSets conf@Conf{assignmode=Exists} (_, cs) = sat conf cs
    noSets conf x = defaultNoSets conf x

type SatGame = SetVariantGame Card
type SatCard = Card
