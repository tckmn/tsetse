#!/usr/bin/env runhaskell

import AllGames.AllGames
import AllGames.SetVariant
import Data.List (subsequences)
import Types
import Util

trial :: (Show card, SetVariant card) => [card] -> SVConf card -> Int -> Int -> Int -> IO ()
trial deck conf n goods total = do
    if total `mod` 100 == 0
       then putStrLn $ (show $ fromIntegral goods / fromIntegral total) ++ "\t" ++ (show $ fromIntegral total)
       else return ()
    cs <- shuffle deck
    let subs = subsequences $ take n cs
    let sets = [s | s <- subs, length s `elem` setSizes conf, checkSet conf s]
    -- if null sets
    --    then return ()
    --    else putStrLn . show $ sets
    trial deck conf n (if null sets then goods else succ goods) (succ total)

nsets :: (Show card, SetVariant card) => [card] -> SVConf card -> Int -> Int -> Int -> IO ()
nsets deck conf n m trials = do
    if trials `mod` 100 == 0
       then putStrLn . show $ trials
       else return ()
    cs <- shuffle deck
    let subs = subsequences $ take n cs
    let sets = [s | s <- subs, length s `elem` setSizes conf, checkSet conf s]
    if length sets == m
       then do
           putStrLn . show $ take n cs
           putStrLn . show $ sets
       else nsets deck conf n m (succ trials)

main = do

    -- let conf = SatConf 6 [4] False (Exactly 2) FromCards
    -- trial (fullDeck conf) conf 8 0 0

    -- let conf = OctaConf Linear
    -- trial (fullDeck conf) conf 12 0 0

    -- let conf = AssetConf Linear
    -- trial (fullDeck conf) conf 9 0 0

    -- let conf = CsetConf
    -- trial (fullDeck conf) conf 12 0 0

    let conf = OctaConf Linear
    nsets (fullDeck conf) conf 9 1 0
