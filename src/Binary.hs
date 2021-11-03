{-# LANGUAGE RecordWildCards #-}

module Binary where

import AllGames
import Data.Binary
import qualified Data.HashMap.Strict as M
import Types hiding (put, get)

instance Binary User

instance Binary GeneralGame where
    put GeneralGame{..} = do put $ fst (desc _game)
                             put _game
                             put _creator
                             put $ show _creation
    get = do
        gname <- get
        let partial = case gname of
                        "A5SET" -> GeneralGame <$> (get :: Get AssetGame)
                        "C53T" -> GeneralGame <$> (get :: Get CsetGame)
                        "FO1D" -> GeneralGame <$> (get :: Get FoidGame)
                        "FOLD" -> GeneralGame <$> (get :: Get FoldGame)
                        "OCTA" -> GeneralGame <$> (get :: Get OctaGame)
                        "S3T2" -> GeneralGame <$> (get :: Get Set2Game)
                        _ -> error "unknown game name in state file???"
        partial <*> get <*> (read <$> get)

instance Binary ServerState where
    put ServerState{..} = do put _users
                             put _nextClient
                             put _nextGame
                             put _password
                             put $ M.toList _games
    get = ServerState <$> pure []
                      <*> get
                      <*> pure 0
                      <*> get
                      <*> get
                      <*> get
                      <*> (M.fromList <$> get)