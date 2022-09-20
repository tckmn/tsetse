{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module AllGames.SetVariant where

import Control.Applicative
import Data.Binary as B
import Data.List (nub, subsequences, elemIndex, permutations)
import Data.Maybe
import Data.Functor
import GHC.Generics (Generic)
import Language.Haskell.TH
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

import GameUtil
import GM
import Types
import Util

import Data.Aeson hiding ((.=))

-- import Debug.Trace
-- debug x = trace (T.unpack . encodeT $ x) x
debug = id

rudebuf :: Int
rudetime :: NominalDiffTime
rudebuf = 3
rudetime = 10

class (Eq card, ToJSON card, FromJSON card, ToJSON (SVConf card), FromJSON (SVConf card), Binary (SVConf card)) => SetVariant card where
    data SVConf card :: *
    name :: card -> Text
    setSizes :: SVConf card -> [Int]
    fullDeck :: SVConf card -> [card]
    checkSet :: SVConf card -> [card] -> Bool
    findSets :: SVConf card -> ([card], [card]) -> [[card]]
    findSets conf (_, cs) =
        [s | s <- subsequences cs
        , length s `elem` setSizes conf
        , checkSet conf s
        ]
    noSets :: SVConf card -> ([card], [card]) -> Bool
    noSets = defaultNoSets
    defaultNoSets :: SVConf card -> ([card], [card]) -> Bool
    defaultNoSets = (null .) . findSets

data Event = Taken ClientId
           | Dealt
           | Added
           deriving Generic

instance Binary Event

data SetVariantGame card =
    SetVariantGame { _deck :: [card]
                   , _cards :: [card]
                   , _events :: [(Event, [card], UTCTime)]
                   , _rudeness :: HashMap ClientId [UTCTime]
                   }

instance Binary card => Binary (SetVariantGame card) where
    put SetVariantGame{..} = do B.put _deck
                                B.put _cards
                                B.put _events
    get = do SetVariantGame <$> B.get
                            <*> B.get
                            <*> B.get
                            <*> pure M.empty

makeLenses ''SetVariantGame
taken :: Getter (SetVariantGame card) [(ClientId, [card], UTCTime)]
taken = events.to (\x -> [(a,b,c) | (Taken a,b,c) <- x])

data Msg card = Claim { idxs :: [Int] }
              | PostClaim { pwd :: Text, i_cards :: [card] }
              | PlusCard
              | GetHistory
              | ExhibitSet { i_password :: Text }
              deriving Generic

data OutMsg card = Cards { o_cards :: [card] }
                 | UserInfo { o_score :: Int }
                 | History { o_history :: [(Text, [card], UTCTime)] }
                 deriving Generic

makeJSON' ''Msg
makeJSON' ''OutMsg

-- torsor helpers

data TorsorConf = Linear | Parallelogram | CommSquare deriving Generic
instance Binary TorsorConf where
    get = do
        x <- B.get :: Get NoConf
        pure Linear
makeJSON ''TorsorConf

class Eq (GroupElement a) => Torsor a where
    type GroupElement a :: *
    (@-) :: a -> a -> GroupElement a

helpful :: Torsor a => (Int -> Bool) -> ([a] -> Bool) -> [a] -> Bool
helpful c f = (c . length) .&&. (liftM3 if' ((> 8) . length) f (any f . permutations))

torsor :: Torsor a => TorsorConf -> [a] -> Bool

torsor Linear = helpful (>= 3) linear
    where linear (a:b:c:xs) = (a @- b) == (b @- c) && linear (b:c:xs)
          linear _ = True

torsor Parallelogram = helpful ((>= 4) .&&. even) para
    where para (a:b:c:d:xs) = (a @- b) == (c @- d) && para (c:d:xs)
          para _ = True

torsor CommSquare = helpful (== 4) comm
    where comm [a,b,c,d] = (a @- b) == (c @- d) && (a @- c) == (b @- d)
          comm _ = False

torsorSizes :: TorsorConf -> [Int]
torsorSizes Linear = [3]
torsorSizes _ = [4]

instance SetVariant card => ToJSON (GConf (SetVariantGame card))
instance SetVariant card => FromJSON (GConf (SetVariantGame card))

instance SetVariant card => Binary (GConf (SetVariantGame card)) where
    put SVConf'{..} = do B.put boardSize
                         B.put dealDelay
                         B.put maxRedeals
                         B.put subconf
    get = SVConf' <$> B.get
                  <*> B.get
                  <*> B.get
                  <*> B.get

-- actions

dealReplace :: SetVariant card => [card] -> Int -> GameIO (SetVariantGame card) ([card], [card])
dealReplace i_cards dealCount = do
    newCards <- uses deck $ take dealCount
    let replaces = zip i_cards $ (Just <$> newCards) ++ repeat Nothing
    cs <- uses cards $ mapMaybe (\c -> fromMaybe (Just c) $ lookup c replaces)
    return (newCards, cs)

dealAdd :: SetVariant card => Int -> GameIO (SetVariantGame card) ([card], [card])
dealAdd dealCount = do
    newCards <- uses deck $ take dealCount
    cs <- uses cards $ (<> newCards)
    return (newCards, cs)

sendDeal :: SetVariant card => Event -> Int -> ([card], [card]) -> GameIO (SetVariantGame card) ()
sendDeal ev dealCount (newCards, cs) = do
    now <- liftIO getCurrentTime
    deck %= drop dealCount
    cards .= cs
    unless (null newCards) $ events <>= [(ev, newCards, now)]
    broadcast $ Cards cs

tryDeal :: SetVariant card => (Int -> GameIO (SetVariantGame card) ([card], [card])) -> Event -> Int -> Int -> GameIO (SetVariantGame card) PostAction
tryDeal fn ev dealCount i = do
    SVConf'{..} <- view rconf
    d <- fn dealCount
    let failed = noSets subconf d
    if failed && i < maxRedeals
       then do
           deck' <- join $ uses deck shuffle
           deck .= deck'
           tryDeal fn ev dealCount $ succ i
       else do
           -- when failed $ broadcast (Toast "failed to guarantee set existence")
           broadcast . Toast $
               if failed
                  then "failed to guarantee set existence"
                  else "redeals: " <> T.pack (show i)
           sendDeal ev dealCount d
           return NewDesc

repeatDeal :: SetVariant card => (Int -> GameIO (SetVariantGame card) ([card], [card])) -> Event -> Int -> GameIO (SetVariantGame card) PostAction
repeatDeal fn ev dealCount = do
    SVConf'{..} <- view rconf
    remaining <- uses deck length
    if dealCount >= remaining
       then do
           d <- fn dealCount
           sendDeal ev dealCount d
           if noSets subconf d
              then do
                  broadcast $ Toast "game over!"
                  return Die
              else return NewDesc
       else tryDeal fn ev dealCount 0

instance (Binary card, SetVariant card) => Game (SetVariantGame card) where

    type GMsg (SetVariantGame card) = Msg card
    data GConf (SetVariantGame card) = SVConf' { boardSize :: Int
                                               , dealDelay :: Int
                                               , maxRedeals :: Int
                                               , subconf :: SVConf card
                                               }
                                     deriving Generic

    new SVConf'{..} _
      | boardSize < 6 = return $ Left "at least 6 cards required"
      | maxRedeals > 100 = return $ Left "at most 100 redeals allowed"
      | otherwise = Right <$> do
          now <- liftIO getCurrentTime
          shuf <- shuffle $ fullDeck subconf
          let (cards, deck) = splitAt boardSize shuf
          return SetVariantGame { _deck = deck
                                , _cards = cards
                                , _events = [(Dealt, cards, now)]
                                , _rudeness = M.empty
                                }

    catchup = do
        cs <- use cards
        send $ Cards cs

    players g = nub $ g^..taken.folded._1

    scores g = foldr (\(k, v, _) ->
        let v' = length v
         in M.alter (Just . maybe v' (+v')) k) M.empty $ g^.taken

    deletable g = null $ g^.taken

    userinfo g cid = toJSON (UserInfo score :: OutMsg card)
        where score = sumOf (taken.folded.filteredBy (_1.only cid)._2.to length) g

    desc g = (name (undefined :: card), (T.pack . show $ length (_deck g) + length (_cards g)) <> " cards left")

    recv Claim{..} = do
        who <- view $ rclient.cid
        when <- liftIO getCurrentTime

        -- don't be rude
        rude <- preuse $ rudeness.at who._Just.ix (rudebuf-1)
        case rude of
          Just t | when `diffUTCTime` t < rudetime -> do
              send $ Toast "too many wrong guesses"
              empty
          _ -> return ()

        -- make sure the request is well-formed
        cs <- use cards
        ts <- use $ taken.folded._2
        let idxs' = nub idxs
            set = [c | idx <- idxs', c <- cs^..ix idx, c `notElem` ts]
        guard $ length idxs' == length set

        -- flash em red if they're not a set
        SVConf'{..} <- view rconf
        unless (checkSet subconf set) $ do
            rudeness.at who %= Just . take rudebuf . (when:) . fromMaybe []
            broadcast $ Highlight who idxs' False
            empty

        -- they're a set! tell everyone
        events <>= [(Taken who, set, when)]
        userlist
        broadcast $ Highlight who idxs' True

        -- wait 5 seconds and clear the cards
        pwd <- view $ rserver.password
        return $ Delayed (dealDelay*1000) (encodeT $ object [ "t" .=> "PostClaim"
                                                            , "pwd" .> pwd
                                                            , "cards" .> set
                                                            ])

    recv PostClaim{..} = do
        checkpwd pwd

        SVConf'{..} <- view rconf
        nboard <- uses cards length
        let dealCount = max 0 $ boardSize - (nboard - length i_cards)
        repeatDeal (dealReplace i_cards) Dealt dealCount

        -- newCards <- deck %%= splitAt dealCount
        -- let replaces = zip i_cards $ (Just <$> newCards) ++ repeat Nothing
        -- cs <- cards <%= mapMaybe (\c -> fromMaybe (Just c) $ lookup c replaces)
        -- broadcast $ Cards cs
        -- return NewDesc

    recv PlusCard = do
        cs <- use cards
        SVConf'{..} <- view rconf

        if noSets subconf ([], cs)
           then repeatDeal dealAdd Added 3
           else send (Toast "there are sets on the board!") $> Done

    recv GetHistory = do
        hist <- use taken
        hist' <- mapM patchu hist
        send $ History hist'
        return Done
            where patchu h@(uid, _, _) = do
                    u <- view $ rserver.byUid uid.uname
                    return $ h & _1 .~ u :: GameIO (SetVariantGame card) (Text, [card], UTCTime)

    recv ExhibitSet{..} = do
        checkpwd i_password

        cs <- use cards
        SVConf'{..} <- view rconf

        send . Toast . encodeT $ map (fromMaybe 0 . flip elemIndex cs) <$> findSets subconf ([], cs)
        return Done

makeCard :: Name -> DecsQ
makeCard t = [d|
    instance Binary $(pure $ ConT t)
    instance FromJSON $(pure $ ConT t) where
        parseJSON = genericParseJSON jsonOpts
    instance ToJSON $(pure $ ConT t) where
        toJSON = genericToJSON jsonOpts
        toEncoding = genericToEncoding jsonOpts
    instance Binary (SVConf $(pure $ ConT t))
    instance FromJSON (SVConf $(pure $ ConT t)) where
        parseJSON = genericParseJSON jsonOpts
    instance ToJSON (SVConf $(pure $ ConT t)) where
        toJSON = genericToJSON jsonOpts
        toEncoding = genericToEncoding jsonOpts
    |]
