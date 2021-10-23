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

import Data.Aeson hiding ((.=))
import qualified Network.WebSockets as WS

import Util
import Types
import GM
import OCWall
import Cset


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
    cid <- case (decodeT greeting, decodeT greeting) of
      (Just Identify{..}, _) -> do
          u <- previewMVar state $ byUid i_cid
          case u of
            Just u | u^.secret == i_secret -> do
                sendWS conn $ Identified (u^.uname)
                return $ Just i_cid
            _ -> do
                sendWS conn $ NotIdentified
                negotiate state conn
                return Nothing
      (_, Just Register{..}) -> do
          uid <- state .&++ nextClient
          secret <- makeSecret
          overMVar state $ users %~ (User { _uid = uid
                                          , _secret = secret
                                          , _uname = i_uname
                                          }:)
          sendWS conn $ Registered uid secret i_uname
          return $ Just uid
      _ -> return Nothing


    -- timeSync conn $ T.cons 'g' stamp
    -- end of negotiation, client is good

    let connect c = do
        logC conn "connected"

        modifyMVar_ state $ \s -> do
            runCatchup c s
            runUserlist c $ s & clients %~ (c:)

        -- main loop
        forever $ do
            msg <- recvWS conn
            modifyMVar_ state $ \s -> runRecv c s msg

    let disconnect c = do
        logC conn "disconnected"
        modifyMVar_ state $ \s ->
            runUserlist c $ s & clients %~ filter ((/= conn) . _conn)

    forM_ cid $ liftM2 finally connect disconnect . flip Client conn

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
    g <- new :: IO CsetGame
    state <- newMVar $ ServerState { _clients = []
                                   , _users = []
                                   , _admins = []
                                   , _nextConn = 0
                                   , _nextClient = 0
                                   , _password = pwd
                                   , _game = GeneralGame g
                                   }
    log "starting server"
    WS.runServer "0.0.0.0" 9255 $ app state
