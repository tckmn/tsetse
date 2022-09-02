{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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
        let partial = $(onGameType 'gname
                (\t -> [| GeneralGame <$> (get :: Get $(t)) <*> get |])
                [| error "unknown game name in state file???" |])
        partial <*> get <*> get <*> get

instance Binary ServerState where
    put ServerState{..} = do put _users
                             put _nextClient
                             put _nextGame
                             put _password
                             put _allowLogins
                             put _games
    get = ServerState <$> pure []
                      <*> get
                      <*> pure 0
                      <*> get
                      <*> get
                      <*> get
                      <*> get
                      <*> get
