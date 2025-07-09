{-# LANGUAGE TemplateHaskell #-}

module AllGames.AllGames
    ( module AllGames.Asset
    , module AllGames.Cece
    , module AllGames.Cset
    , module AllGames.Foid
    , module AllGames.Fold
    , module AllGames.OCon
    , module AllGames.Octa
    , module AllGames.Sat
    , module AllGames.Set
    , module AllGames.Set2
    , onGameType
    ) where

import AllGames.Asset
import AllGames.Cece
import AllGames.Cset
import AllGames.Foid
import AllGames.Fold
import AllGames.OCon
import AllGames.Octa
import AllGames.Sat
import AllGames.Set
import AllGames.Set2

import qualified Types
import qualified Data.Text as T

import Language.Haskell.TH

namesToTypes :: [(String, Name)]
namesToTypes = [ (T.unpack . fst $ Types.desc (undefined :: AssetGame), ''AssetGame)
               , (T.unpack . fst $ Types.desc (undefined :: CeceGame),  ''CeceGame)
               , (T.unpack . fst $ Types.desc (undefined :: CsetGame),  ''CsetGame)
               , (T.unpack . fst $ Types.desc (undefined :: FoidGame),  ''FoidGame)
               , (T.unpack . fst $ Types.desc (undefined :: FoldGame),  ''FoldGame)
               , (T.unpack . fst $ Types.desc (undefined :: OConGame),  ''OConGame)
               , (T.unpack . fst $ Types.desc (undefined :: OctaGame),  ''OctaGame)
               , (T.unpack . fst $ Types.desc (undefined :: SatGame),   ''SatGame)
               , (T.unpack . fst $ Types.desc (undefined :: SetGame),   ''SetGame)
               , (T.unpack . fst $ Types.desc (undefined :: SectGame),  ''SectGame)
               ]

onGameType :: Name -> (TypeQ -> ExpQ) -> ExpQ -> ExpQ
onGameType name ymatch nmatch =
    caseE (varE name) $
      map (\(n,t) ->
          match (litP $ stringL n)
                (normalB . ymatch $ conT t)
                []
      ) namesToTypes ++
      [
          match (varP $ mkName "unk")
                (normalB nmatch)
                []
      ]
