#!/usr/bin/env runhaskell

import AllGames.AllGames
import AllGames.SetVariant
import Data.List (subsequences)
import Types
import Util

trial :: (Show card, SetVariant card) => [card] -> SVConf card -> Int -> Int -> Int -> IO ()
trial deck conf n goods total = do
    if total `mod` 100 == 0
       then putStrLn . show $ fromIntegral goods / fromIntegral total
       else return ()
    cs <- shuffle deck
    let subs = subsequences $ take n cs
    let sets = [s | s <- subs, length s `elem` setSizes (head deck), checkSet conf s]
    -- if null sets
    --    then return ()
    --    else putStrLn . show $ sets
    trial deck conf n (if null sets then goods else succ goods) (succ total)

main = do
    let conf = Conf 6 [4] False (Exactly 2) FromCards
    trial (fullDeck conf :: [SatCard]) conf 8 0 0
