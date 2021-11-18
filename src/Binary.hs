{-# LANGUAGE RecordWildCards #-}

module Binary where

import AllGames.AllGames
import AllGames.SetVariant
import Data.Binary
import qualified Data.HashMap.Strict as M
import Types hiding (put, get)

instance Binary User

instance Binary GeneralGame where
    put GeneralGame{..} = do put $ fst (desc _game)
                             put _game
                             put _gconf
                             put _creator
                             put _creation
                             put _dead
    get = do
        gname <- get
        let partial = case gname of
                        "A5SET" -> GeneralGame <$> (get :: Get AssetGame) <*> (pure $ SVConf' 10 (NoConf ()))
                        "C53T" -> GeneralGame <$> (get :: Get CsetGame) <*> (pure $ SVConf' 12 (NoConf ()))
                        "FO1D" -> GeneralGame <$> (get :: Get FoidGame) <*> (pure $ SVConf' 12 (NoConf ()))
                        "FOLD" -> GeneralGame <$> (get :: Get FoldGame) <*> (pure $ SVConf' 12 (NoConf ()))
                        "OCTA" -> GeneralGame <$> (get :: Get OctaGame) <*> (pure $ SVConf' 9 (NoConf ()))
                        "S3CT" -> GeneralGame <$> (get :: Get SectGame) <*> (pure $ SVConf' 10 (NoConf ()))
                        "C3C3" -> GeneralGame <$> (get :: Get CeceGame) <*> (pure $ SVConf' 12 (NoConf ()))
                        "SAT" -> GeneralGame <$> (get :: Get SatGame) <*> (pure $ SVConf' 8 (NoConf ()))
                        _ -> error "unknown game name in state file???"
        partial <*> get <*> get <*> get

instance Binary ServerState where
    put ServerState{..} = do put _users
                             put _nextClient
                             put _nextGame
                             put _password
                             put _games
    get = ServerState <$> pure []
                      <*> get
                      <*> pure 0
                      <*> get
                      <*> get
                      <*> get
                      <*> get
