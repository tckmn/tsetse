#!/usr/bin/env runhaskell

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (log)

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Data.Char (isSpace)
import Data.Maybe
import Data.Tuple (swap)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as M

import Data.Aeson hiding ((.=))
import Data.Binary as B
import qualified Network.WebSockets as WS

import AllGames.AllGames
import Binary
import GM
import GameUtil
import Types
import Util

-- random utility functions (first generic, then codebase-specific)

timeMillis :: Integral a => IO a
timeMillis = round . (1000*) <$> getPOSIXTime

timeSync :: Connection -> Text -> IO ()
timeSync conn echo = timeMillis >>=
    sendWS conn . T.concat . ([echo, "/"] ++) . pure . T.pack . show

-- i'm really sorry for this but i couldn't resist
(.&++) :: Enum t => MVar s -> Lens' s t -> IO t
state .&++ lens = modifyMVar state $ return . swap . (lens <<%~ succ)

-- main logic

app :: MVar ServerState -> WS.ServerApp
app state pending = do
    conn <- WS.acceptRequest pending
    connid <- state .&++ nextConn
    WS.withPingThread conn 30 (pure ()) $ negotiate state (Connection conn connid)

previewMVar v lens = withMVar v $ return . preview lens
overMVar v lens = modifyMVar_ v $ return . lens
nameok state name = do
    sameName <- withMVar state $
        return . (^..users.traversed.filteredBy (uname.only name))
    return $ (not . T.null . T.filter (not . isSpace)) name && null sameName

negotiate :: MVar ServerState -> Connection -> IO ()
negotiate state conn = do
    logC conn "negotiate attempt"

    greeting <- recvWS conn
    cid <- case decodeT greeting of
      Just Identify{..} -> do
          u <- previewMVar state $ byUid i_cid
          case u of
            Just u | u^.secret == i_secret -> do
                sendWS conn $ Identified (u^.uname)
                return $ Just i_cid
            _ -> do
                sendWS conn $ NotIdentified
                negotiate state conn
                return Nothing
      Just Register{..} -> do
          isok <- nameok state i_uname
          if isok
            then do
                uid <- state .&++ nextClient
                secret <- makeSecret
                overMVar state $ users %~ (User { _uid = uid
                                                , _secret = secret
                                                , _uname = i_uname
                                                }:)
                sendWS conn $ Registered uid secret i_uname
                return $ Just uid
            else do
                sendWS conn $ NotRegistered
                negotiate state conn
                return Nothing
      Nothing -> return Nothing

    forM_ cid $ \cid -> play state conn cid (-1)

play :: MVar ServerState -> Connection -> ClientId -> GameId -> IO ()
play state conn cid gid = do
    let c = Client conn cid gid
    newgid <- catches (connect state c `finally` disconnect state c)
                      [ Handler (\(e :: WS.ConnectionException) -> throw e)
                      , Handler (\(e :: SomeException) -> do
                          logC c $ "**exception " <> T.pack (show (e :: SomeException))
                          throw e)
                      ]
    play state conn cid newgid

newgame :: Game g => MVar ServerState -> Client -> GConf g -> g -> IO GameId
newgame state c conf g = do
    gid <- state .&++ nextGame
    now <- getCurrentTime
    overMVar state $ games.at gid .~ Just GeneralGame { _game = g
                                                      , _gconf = conf
                                                      , _creator = c^.cid
                                                      , _creation = now
                                                      , _dead = False
                                                      }
    withMVar state runGameList
    return gid

connect :: MVar ServerState -> Client -> IO GameId
connect state c = do
    logC c "connected"

    modifyMVar_ state $ \s -> do
        runGameType c s
        runCatchup c s
        runUserlist c $ s & clients %~ (c:)

    let toast = sendWS c . Toast

    -- main loop
    let loop = do
        msg <- recvWS c
        case decodeT msg of
          Just JoinGame{..} -> return i_gid
          Just (CreateGame "C53T" conf)  -> case fromJSON conf of
                                              Success conf -> (new conf :: IO CsetGame) >>= newgame state c conf
                                              _ -> loop
          Just (CreateGame "FO1D" conf)  -> case fromJSON conf of
                                              Success conf -> (new conf :: IO FoidGame) >>= newgame state c conf
                                              _ -> loop
          Just (CreateGame "S3CT" conf)  -> case fromJSON conf of
                                              Success conf -> (new conf :: IO SectGame) >>= newgame state c conf
                                              _ -> loop
          Just (CreateGame "A5SET" conf) -> case fromJSON conf of
                                              Success conf -> (new conf :: IO AssetGame) >>= newgame state c conf
                                              _ -> loop
          Just (CreateGame "OCTA" conf)  -> case fromJSON conf of
                                              Success conf -> (new conf :: IO OctaGame) >>= newgame state c conf
                                              _ -> loop
          Just (CreateGame "FOLD" conf)  -> case fromJSON conf of
                                              Success conf -> (new conf :: IO FoldGame) >>= newgame state c conf
                                              _ -> loop
          Just (CreateGame "C3C3" conf)  -> case fromJSON conf of
                                              Success conf -> (new conf :: IO CeceGame) >>= newgame state c conf
                                              _ -> loop
          Just (CreateGame "SAT" conf)   -> case fromJSON conf of
                                              Success conf -> (new conf :: IO SatGame) >>= newgame state c conf
                                              _ -> loop
          Just (CreateGame unk _) -> do
              toast $ "unknown game type " <> unk
              loop
          Just DeleteGame{..} -> do
              who <- previewMVar state $ games.at i_gid._Just.creator
              if who == Just (c^.cid)
                 then do overMVar state $ games.at i_gid .~ Nothing
                         withMVar state runGameList
                 else toast "you can only delete your own games"
              loop
          Just GetScores -> do
              let score GeneralGame{..} =
                    amend (M.unionWith (+) (scores _game) . fromMaybe M.empty) (desc _game^._1)
              let names s = mapKeys (\uid -> s^.byUid uid.uname)
              withMVar state $ \s ->
                  sendWS c . Scores . fmap (names s) $ M.foldr score M.empty (s^.games)
              loop
          Just Uname{..} -> do
              isok <- nameok state i_uname
              if isok
                 then do
                     overMVar state $ byUid (c^.cid).uname .~ i_uname
                     -- TODO notify other players
                     sendWS c $ Identified i_uname
                 else toast "can't change to that name"
              loop
          -- admin
          Just (SaveState pwd) -> do
              withMVar state $ \s ->
                  if pwd == s^.password
                     then do
                         liftIO . B.encodeFile "state" $ s
                         toast "saved!"
                     else toast "wrong password"
              loop
          -- Nothing and unimplemented
          _ -> do
              let gamemsg msg = do
                  post <- modifyMVar state $ \s -> runRecv c s msg
                  case post of
                    Done -> return ()
                    NewDesc -> withMVar state runGameList
                    Die -> do
                        overMVar state $ cgame c._Just.dead .~ True
                        withMVar state runGameList
                    Delayed{..} -> void . forkIO $ do
                        threadDelay delay
                        gamemsg msg
              gamemsg msg
              loop

    loop

disconnect :: MVar ServerState -> Client -> IO ()
disconnect state c = do
    logC c "disconnected"
    modifyMVar_ state $ \s ->
        runUserlist c $ s & clients %~ filter ((c^.conn /=) . _conn)

initstate :: IOException -> IO ServerState
initstate _ = do
    putStr "admin password: "
    pwd <- T.getLine
    return ServerState { _clients = []
                       , _users = []
                       , _nextConn = 0
                       , _nextClient = 0
                       , _nextGame = 0
                       , _password = pwd
                       , _games = M.empty
                       }

main :: IO ()
main = do
    state <- newMVar =<< B.decodeFile "state" `catch` initstate
    log "starting server"
    WS.runServer "0.0.0.0" 5354 $ app state
