#!/usr/bin/env runhaskell

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

import Prelude hiding (log)

import Control.Applicative
import Control.Concurrent (MVar, newMVar, withMVar, modifyMVar, modifyMVar_)
import Control.Exception (catch, finally, IOException)
import Control.Monad
import Data.Char (isUpper, isAscii, isSpace, isDigit)
import Data.Functor
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


idLength = 5
secretLength = 10
pwdfile = "pwd"


-- random utility functions (first generic, then codebase-specific)

between :: Ord a => a -> a -> a -> Bool
between a b x = a <= x && x <= b

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs:chunksOf n (drop n xs)

timeMillis :: Integral a => IO a
timeMillis = round . (1000*) <$> getPOSIXTime

-- reqa :: ServerState -> ClientId -> IO ServerState -> IO ServerState
-- reqa s@ServerState{admins} cid blk = if cid `elem` admins then blk else return s

-- reqp :: ServerState -> ClientId -> IO ServerState -> IO ServerState
-- reqp s@ServerState{players} cid blk = if cid `elem` players then blk else return s

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
        runCatchup c s
        runUserlist c $ s & clients %~ (c:)

    -- main loop
    let loop = do
        msg <- recvWS c
        case decodeT msg of
          Just JoinGame{..} -> return i_gid
          Just (CreateGame "c53t") -> do
              g <- new :: IO CsetGame
              gid <- state .&++ nextGame
              overMVar state $ games.at gid .~ Just (GeneralGame g)
              withMVar state $ \s -> mapM_ (flip runGameList s) (s^..byGid (-1))
              return gid
          Just (CreateGame unk) -> do
              sendWS c . Toast $ "unknown game type " <> unk
              loop
          Nothing -> do
              modifyMVar_ state $ \s -> runRecv c s msg
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
    T.writeFile pwdfile pwd
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
