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
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Map as M
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
          u <- previewMVar state $ users.folded.filtered ((==i_cid) . _uid)
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
          return $ Just uid
      _ -> return Nothing


    -- timeSync conn $ T.cons 'g' stamp
    -- end of negotiation, client is good

    let connect cid = do
        logC conn "connected"
        let c = Client cid conn

        modifyMVar_ state $ \s@ServerState{_game} -> do
            -- catch up
            runGameIO catchup (c, s) _game
            -- when (cid `elem` admins)  $ sendWS conn $ encodeAdmin True
            -- when (cid `elem` players) $ sendWS conn $ encodePlayer True
            -- when (not $ null wall)    $ sendWS conn $ encodeWall startTime duration wall
            -- when (not $ null groups)  $ sendWS conn $ encodeGuess True groups
            -- when (strikes /= 3)       $ sendWS conn $ encodeStrikes strikes
            -- add client to list (and tell admins)
            withClientsUpdate $ s & clients %~ (c:)
                                  -- & secrets %~ M.insert cid sec

        -- main loop
        forever $ do
            msg <- recvWS conn
            modifyMVar_ state $ \s@ServerState{..} -> do
                (_, game') <- runGameIO (sequence_ $ recvT msg) (c, s) _game
                -- return $ s & game .= game'
                -- return $ s { game = game' }
                return $ ServerState { _clients
                                     , _users
                                     , _players
                                     , _admins
                                     , _nextConn
                                     , _nextClient
                                     , _password
                                     , _game = game'
                                     }
            -- uncurry (play state c) $ T.splitAt 1 msg

    let disconnect = do
        logC conn "disconnected"
        modifyMVar_ state $ \s ->
            withClientsUpdate $ s & clients %~ filter ((/= conn) . _conn)

    forM_ cid $ \cid -> connect cid `finally` disconnect

{-

play :: MVar ServerState -> Client -> Text -> Text -> IO ()

-- requesting admin access
play state (Client cid conn) "a" pwd =
    modifyMVar_ state $ \s@ServerState{password,admins} -> do
        let good = pwd == password
        sendWS conn $ encodeAdmin good
        if good then withClientsUpdate s { admins = cid:admins } else return s

-- admin submitted a new wall
play state (Client cid conn) "w" walldata =
    modifyMVar_ state $ \s@ServerState{clients,wall,groups,strikes,startTime,duration} -> reqa s cid $ do
        -- jesus christ, what a line
        new <- sequence $ liftM2 zip (shuffle [0..15]) . pure <$> parseWall walldata
        -- tell everyone about it
        now <- timeMillis
        sequence_ $ broadcast clients . encodeWall now duration <$> new
        -- make sure to keep this in sync with ABC in netwall.js
        return s { wall      = fromMaybe wall               new
                 , groups    = fromMaybe groups    $ []  <$ new
                 , strikes   = fromMaybe strikes   $ 3   <$ new
                 , startTime = fromMaybe startTime $ now <$ new
                 }
    where parseWall s = let cells = filter (not . T.null) . map strip . T.splitOn "\n" $ chomp s
                         in cells <$ guard (length cells == 16)

-- admin cleared wall
play state (Client cid conn) "W" _ =
    modifyMVar_ state $ \s@ServerState{clients,wall,groups,strikes,startTime,duration} -> reqa s cid $ do
        broadcast clients "W"
        -- make sure to keep this in sync with DEF in netwall.js
        return s { wall      = []
                 , groups    = []
                 , strikes   = 3
                 , startTime = 0
                 }

-- (un)make someone an admin
-- ugly code duplication between this and the "P" case, but whatever
play state (Client cid conn) "A" req =
    modifyMVar_ state $ \s@ServerState{clients,admins} -> reqa s cid $ do
        let (action,target) = T.splitAt 1 req
            yes = action == "1"
        -- we won't bother checking if target is real because we trust admins
        broadcast (withCid target clients) $ encodeAdmin yes
        withClientsUpdate s { admins = [target | yes] ++ filter (/=target) admins }

-- (un)make someone a player
play state (Client cid conn) "P" req =
    modifyMVar_ state $ \s@ServerState{clients,players} -> reqa s cid $ do
        let (action,target) = T.splitAt 1 req
            yes = action == "1"
        -- we won't bother checking if target is real because we trust admins
        broadcast (withCid target clients) $ encodePlayer yes
        withClientsUpdate s { players = [target | yes] ++ filter (/=target) players }

-- player submitted a guess
play state (Client cid conn) "g" guess =
    modifyMVar_ state $ \s@ServerState{clients,groups} -> reqp s cid $ do
        now <- timeMillis
        fromMaybe (return s) (makeGuess s <$> parseGuess s now guess)
    where parseGuess s@ServerState{groups,strikes,startTime,duration} now guess = do
              guard $ strikes /= 0
              guard $ now - startTime < (fromIntegral duration)*1000
              xs <- sequence $ decimal <$> T.splitOn "/" guess
              guard $ all ($xs) [(==4) . length,
                                 liftM2 (==) id nub,
                                 all (liftM2 (&&) (between 0 15) (`notElem` groups))]
              return xs
          makeGuess s@ServerState{clients,groups,wall,strikes} guess =
              if sort guess `elem` (map sort . chunksOf 4 $ fst <$> wall)
                 then do
                     let groups' = groups ++ guess
                         groups'' = if length groups' == 12
                                       then groups' ++ filter (`notElem` groups') [0..15]
                                       else groups'
                     broadcast clients $ encodeGuess True groups''
                     return s { groups = groups'' }
                 else do
                     let strike = length groups == 8
                         strikes' = strikes - if strike then 1 else 0
                     broadcast clients $ encodeGuess False guess
                     when strike $ broadcast clients $ encodeStrikes strikes'
                     return s { strikes = strikes' }

-- timesync
play state (Client _ conn) "t" stamp = timeSync conn $ T.cons 't' stamp

play _ (Client _ conn) _ _ = logC conn "misbehaving client??"



encodeYN :: Char -> Bool -> Text
encodeYN ch yes = T.pack $ ch:(if yes then "1" else "0")
encodeAdmin :: Bool -> Text
encodeAdmin = encodeYN 'a'
encodePlayer :: Bool -> Text
encodePlayer = encodeYN 'p'
encodeWall :: Integer -> Int -> [(Int,Text)] -> Text
encodeWall st du = ('w' `T.cons`) . T.intercalate "\n" . w st . w du . map snd . sortOn fst
    where w x = (T.pack (show x):)
encodeGuess :: Bool -> [Int] -> Text
encodeGuess yes idxs = T.pack $ (if yes then 'g' else 'G'):intercalate "/" (show <$> idxs)
encodeStrikes :: Int -> Text
encodeStrikes = T.pack . ('s':) . show
encodeClients :: [Client] -> Map ClientId Text -> [ClientId] -> [ClientId] -> Text
encodeClients clients secrets players admins =
    ('c' `T.cons`) . T.intercalate "/" . map stringify . sortOn sortPred $ M.toList secrets
    where sortPred (cid,sec) = fromMaybe (length clients) $ findIndex ((==cid) . clientId) clients
          stringify (cid,sec) = T.concat [fmtCount . length . withCid cid $ clients,
                                          fmtYN "p" (cid `elem` players),
                                          fmtYN "a" (cid `elem` admins), " ",
                                          cid, " ",
                                          T.take 5 sec, " ", T.drop 5 sec]
          fmtCount n | n > 9 = "*"
                     | n > 0 = T.pack $ show n
                     | otherwise = " "
          fmtYN s True  = s
          fmtYN s False = " "

-}

withClientsUpdate :: ServerState -> IO ServerState
withClientsUpdate s@ServerState{..} = return () $> s
    -- forM_ (filter ((`elem` _admins) . _cid) _clients)
    --       (flip sendWS clientsEnc . _conn) $> s
    -- where clientsEnc = "" -- encodeClients clients secrets players admins


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
    g <- new
    state <- newMVar $ ServerState { _clients = []
                                   , _users = []
                                   , _players = []
                                   , _admins = []
                                   , _nextConn = 0
                                   , _nextClient = 0
                                   , _password = pwd
                                   , _game = g :: CsetGame
                                   }
    log "starting server"
    WS.runServer "0.0.0.0" 9255 $ app state