#!/usr/bin/env runhaskell

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

import Prelude hiding (log)

import Control.Applicative
import Control.Concurrent
import Control.Exception (catch, finally, IOException)
import Data.Char (isSpace)
import Data.Tuple (swap)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as M

import Data.Aeson hiding ((.=))
import qualified Network.WebSockets as WS

import GM
import GameUtil
import Types
import Util

import Cset
import OCWall


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
          sameName <- withMVar state $
              return . (^..users.traversed.filteredBy (uname.only i_uname))
          if T.null (T.filter (not . isSpace) i_uname) || not (null sameName)
            then do
                sendWS conn $ NotRegistered
                negotiate state conn
                return Nothing
            else do
                uid <- state .&++ nextClient
                secret <- makeSecret
                overMVar state $ users %~ (User { _uid = uid
                                                , _secret = secret
                                                , _uname = i_uname
                                                }:)
                sendWS conn $ Registered uid secret i_uname
                return $ Just uid
      Nothing -> return Nothing

    forM_ cid $ \cid -> play state conn cid (-1)

play :: MVar ServerState -> Connection -> ClientId -> GameId -> IO ()
play state conn cid gid = do
    let c = Client conn cid gid
    newgid <- (connect state c) `finally` (disconnect state c)
    play state conn cid newgid

connect :: MVar ServerState -> Client -> IO GameId
connect state c = do
    logC c "connected"

    modifyMVar_ state $ \s -> do
        runGameType c s
        runCatchup c s
        runUserlist c $ s & clients %~ (c:)

    -- main loop
    let loop = do
        msg <- recvWS c
        case decodeT msg of
          Just JoinGame{..} -> return i_gid
          Just (CreateGame "C53T") -> do
              g <- new :: IO CsetGame
              gid <- state .&++ nextGame
              now <- getCurrentTime
              overMVar state $ games.at gid .~ Just GeneralGame { _game = g
                                                                , _creator = c^.cid
                                                                , _creation = now
                                                                }
              withMVar state runGameList
              return gid
          Just (CreateGame unk) -> do
              sendWS c . Toast $ "unknown game type " <> unk
              loop
          Nothing -> do
              let gamemsg msg = do
                  post <- modifyMVar state $ \s -> runRecv c s msg
                  case post of
                    Done -> return ()
                    NewDesc -> withMVar state runGameList
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

setpass :: IOException -> IO Text
setpass _ = do
    putStr "please set an admin password: "
    pwd <- T.getLine
    T.writeFile "pwd" pwd
    return pwd

main :: IO ()
main = do
    let chomp = T.reverse . T.dropWhile (=='\n') . T.reverse
    pwd <- (chomp <$> T.readFile "pwd") `catch` setpass
    state <- newMVar $ ServerState { _clients = []
                                   , _users = []
                                   , _admins = []
                                   , _nextConn = 0
                                   , _nextClient = 0
                                   , _nextGame = 0
                                   , _password = pwd
                                   , _games = M.empty
                                   }
    log "starting server"
    WS.runServer "0.0.0.0" 9255 $ app state
