#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (log)

import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar, modifyMVar_)
import Control.Exception (catch, finally, IOException)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Char (isUpper, isAscii, isSpace, isDigit)
import Data.Functor
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)
import System.Random (randomRIO)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

import qualified Network.WebSockets as WS


type ClientId = Text
data Connection = Connection WS.Connection Int
instance Eq Connection where
    (Connection _ n1) == (Connection _ n2) = n1 == n2

data Client = Client ClientId Connection
clientId   (Client cid _)  = cid
clientConn (Client _ conn) = conn
withCid cid = filter ((==cid) . clientId)

data ServerState = ServerState { clients :: [Client]
                               , secrets :: Map ClientId Text
                               , players :: [ClientId]
                               , admins :: [ClientId]
                               , nextConn :: Int
                               , password :: Text
                               , wall :: [(Int,Text)]
                               , groups :: [Int]
                               , strikes :: Int
                               , startTime :: Integer
                               , duration :: Int
                               }

idLength = 5
secretLength = 10
pwdfile = "pwd"


-- random utility functions (first generic, then codebase-specific)

chomp :: Text -> Text
chomp = T.reverse . T.dropWhile (=='\n') . T.reverse

strip :: Text -> Text
strip = T.reverse . T.dropWhile isSpace . T.reverse . T.dropWhile isSpace

shuffle :: [a] -> IO [a]
shuffle [] = pure []
shuffle xs = do
    idx <- randomRIO (0, length xs - 1)
    let (left, (x:right)) = splitAt idx xs
    (x:) <$> shuffle (left++right)

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

decimal :: Integral a => Text -> Maybe a
decimal = fmap fst . hush . T.decimal

between :: Ord a => a -> a -> a -> Bool
between a b x = a <= x && x <= b

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs:chunksOf n (drop n xs)

timeMillis :: Integral a => IO a
timeMillis = round . (1000*) <$> getPOSIXTime

reqa :: ServerState -> ClientId -> IO ServerState -> IO ServerState
reqa s@ServerState{admins} cid blk = if cid `elem` admins then blk else return s

reqp :: ServerState -> ClientId -> IO ServerState -> IO ServerState
reqp s@ServerState{players} cid blk = if cid `elem` players then blk else return s

timeSync :: Connection -> Text -> IO ()
timeSync conn echo = timeMillis >>=
    sendWS conn . T.concat . ([echo, "/"] ++) . pure . T.pack . show

-- logging (not saved to a file, use tee)

log :: String -> IO ()
log s = getZonedTime >>= putStrLn . (++s) . formatTime defaultTimeLocale "[%F %T] "

logC :: Connection -> String -> IO ()
logC (Connection _ connid) s = log $ pad connid ++ ": " ++ s
    where pad = reverse . take 5 . (++repeat '0') . reverse . show


-- sending and receiving websocket messages

sendWS :: Connection -> Text -> IO ()
sendWS c@(Connection conn connid) t = do
    logC c $ "SEND " ++ T.unpack t
    WS.sendTextData conn t

recvWS :: Connection -> IO Text
recvWS c@(Connection conn connid) = do
    t <- WS.receiveData conn
    logC c $ "RECV " ++ T.unpack t
    return t

broadcast :: [Client] -> Text -> IO ()
broadcast clients msg = forM_ clients $ flip sendWS msg . clientConn


-- main logic

app :: MVar ServerState -> WS.ServerApp
app state pending = do
    conn <- WS.acceptRequest pending
    connid <- modifyMVar state $
        \s@ServerState{nextConn} -> return (s{nextConn=nextConn+1}, nextConn)
    WS.withPingThread conn 30 (pure ()) $ negotiate state (Connection conn connid)


negotiate :: MVar ServerState -> Connection -> IO ()
negotiate state conn = fmap (maybe () id) . runMaybeT $ do
    lift $ logC conn "negotiate attempt"
    (key, stamp) <- lift $ T.splitAt (idLength + secretLength) <$> recvWS conn
    ServerState{secrets} <- lift $ readMVar state

    lift . when (T.any (not . liftM2 (&&) isUpper isAscii) key ||
                 T.null stamp || T.any (not . isDigit) stamp) $ do
        logC conn "misbehaving client??"
        sendWS conn "e"
        guard False

    let (cid, sec) = T.splitAt idLength key

    lift . when (maybe False (/= sec) $ M.lookup cid secrets) $ do
        logC conn "client id clash, trying again"
        sendWS conn "r"
        negotiate state conn
        guard False

    lift $ timeSync conn $ T.cons 'g' stamp
    -- end of negotiation, client is good

    let connect = do
        logC conn "connected"
        let c = Client cid conn
        modifyMVar_ state $ \s@ServerState{..} -> do
            -- catch up
            when (cid `elem` admins)  $ sendWS conn $ encodeAdmin True
            when (cid `elem` players) $ sendWS conn $ encodePlayer True
            when (not $ null wall)    $ sendWS conn $ encodeWall startTime duration wall
            when (not $ null groups)  $ sendWS conn $ encodeGuess True groups
            when (strikes /= 3)       $ sendWS conn $ encodeStrikes strikes
            -- add client to list (and tell admins)
            withClientsUpdate s { clients = c:clients
                                , secrets = M.insert cid sec secrets }
        -- main loop
        forever $ do
            msg <- recvWS conn
            uncurry (play state c) $ T.splitAt 1 msg

    let disconnect = do
        logC conn "disconnected"
        modifyMVar_ state $ \s@ServerState{clients,secrets} ->
            withClientsUpdate s { clients = filter ((/= conn) . clientConn) clients }

    lift $ connect `finally` disconnect


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
encodeWall st du = ('w' `T.cons`) . T.intercalate "\n" . (T.pack (show st):) . (T.pack (show du):) . map snd . sortOn fst
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

withClientsUpdate :: ServerState -> IO ServerState
withClientsUpdate s@ServerState{clients,secrets,players,admins} =
    forM_ (filter ((`elem` admins) . clientId) clients)
          (flip sendWS clientsEnc . clientConn) $> s
    where clientsEnc = encodeClients clients secrets players admins


setpass :: IOException -> IO Text
setpass _ = do
    putStr "please set an admin password: "
    pwd <- T.getLine
    T.writeFile pwdfile pwd
    return pwd

main :: IO ()
main = do
    pwd <- (chomp <$> T.readFile "pwd") `catch` setpass
    state <- newMVar $ ServerState { clients = []
                                   , secrets = M.empty
                                   , players = []
                                   , admins = []
                                   , nextConn = 0
                                   , password = pwd
                                   , wall = []
                                   , groups = []
                                   , strikes = 3
                                   , startTime = 0
                                   , duration = 150
                                   }
    log "starting server"
    WS.runServer "0.0.0.0" 9255 $ app state
