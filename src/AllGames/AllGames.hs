{-# LANGUAGE TemplateHaskell #-}

module AllGames.AllGames
    ( module AllGames.Asset
    , module AllGames.Cece
    , module AllGames.Cset
    , module AllGames.Foid
    , module AllGames.Fold
    , module AllGames.OCWall
    , module AllGames.Octa
    , module AllGames.Sat
    , module AllGames.Set2
    , namesToTypes
    ) where

import AllGames.Asset
import AllGames.Cece
import AllGames.Cset
import AllGames.Foid
import AllGames.Fold
import AllGames.OCWall
import AllGames.Octa
import AllGames.Sat
import AllGames.Set2

import Language.Haskell.TH

namesToTypes :: [(String, Name)]
namesToTypes = [ ("A5SET", ''AssetGame)
               , ("C53T",  ''CsetGame)
               , ("FO1D",  ''FoidGame)
               , ("FOLD",  ''FoldGame)
               , ("OCTA",  ''OctaGame)
               , ("S3CT",  ''SectGame)
               , ("C3C3",  ''CeceGame)
               , ("SAT",   ''SatGame)
               ]
