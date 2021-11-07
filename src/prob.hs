#!/usr/bin/env runhaskell

import AllGames
import Data.List (subsequences)
import Types
import SetVariant
import Util

trial :: (Show card, SetVariant card) => [card] -> Int -> Int -> Int -> IO ()
trial deck n goods total = do
    if total `mod` 1000 == 0
       then putStrLn . show $ fromIntegral goods / fromIntegral total
       else return ()
    cs <- shuffle deck
    let subs = subsequences $ take n cs
    let sets = [s | s <- subs, length s `elem` setSizes (head deck), checkSet s]
    -- if null sets
    --    then return ()
    --    else putStrLn . show $ sets
    trial deck n (if null sets then goods else succ goods) (succ total)

main = do
    trial (fullDeck :: [CeceCard]) 12 0 0
