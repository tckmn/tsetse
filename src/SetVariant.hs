{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SetVariant where

import Control.Applicative
import Data.Binary as B
import Data.List (nub, subsequences)
import Data.Maybe
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

import GameUtil
import GM
import Types hiding ((.>))
import Util

import Data.Aeson hiding ((.=))
import qualified Data.Aeson as Ae
(.>) :: (KeyValue kv, ToJSON v) => Text -> v -> kv
(.>) = (Ae..=)
(.=>) :: KeyValue kv => Text -> Text -> kv
(.=>) = (Ae..=)

rudebuf :: Int
rudetime :: NominalDiffTime
rudebuf = 3
rudetime = 10

class (Eq card, ToJSON card, FromJSON card) => SetVariant card where
    name :: card -> Text
    boardSize :: card -> Int
    setSizes :: card -> [Int]
    fullDeck :: [card]
    checkSet :: [card] -> Bool

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

instance SetVariant card => FromJSON (Msg card) where
    parseJSON (Object v) = do
        t <- v .: "t"
        case t of
          String "Claim" -> Claim <$> v .: "idxs"
          String "PostClaim" -> PostClaim <$> v .: "pwd" <*> v .: "cards"
          String "PlusCard" -> pure PlusCard
          String "GetHistory" -> pure GetHistory
          _ -> empty
    parseJSON _ = empty

data OutMsg card = Cards { o_cards :: [card] }
                 | UserInfo { o_score :: Int }
                 | History { o_history :: [(Text, [card], UTCTime)] }

instance SetVariant card => ToJSON (OutMsg card) where
    toJSON Cards{..}    = object ["t" .=> "Cards",    "cards"   .> o_cards]
    toJSON UserInfo{..} = object ["t" .=> "UserInfo", "score"   .> o_score]
    toJSON History{..}  = object ["t" .=> "History",  "history" .> o_history]

nosets :: forall card. SetVariant card => [card] -> Bool
nosets cs = null [s | s <- subsequences cs
                 , length s `elem` setSizes (undefined :: card)
                 , checkSet s
                 ]

instance (Binary card, SetVariant card) => Game (SetVariantGame card) (Msg card) where

    new = do
        now <- liftIO getCurrentTime
        shuf <- shuffle fullDeck
        let (cards, deck) = splitAt (boardSize (undefined :: card)) shuf
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

    userinfo g cid = toJSON (UserInfo score :: OutMsg card)
        where score = sumOf (taken.folded.filteredBy (_1.only cid)._2.to length) g

    desc g = (name (undefined :: card), (T.pack . show $ length (_deck g) + length (_cards g)) <> " cards left")

    recv Claim{..} = do
        who <- view $ _1.cid
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
            set = [c | idx <- idxs', let Just c = cs^?ix idx, c `notElem` ts]
        guard $ length idxs' == length set

        -- flash em red if they're not a set
        unless (checkSet set) $ do
            rudeness.at who %= Just . take rudebuf . (when:) . fromMaybe []
            send $ Highlight idxs' False
            empty

        -- they're a set! tell everyone
        events <>= [(Taken who, set, when)]
        broadcast $ Highlight idxs' True
        userlist

        -- wait 5 seconds and clear the cards
        pwd <- view $ _2.password
        return $ Delayed 5000000 (encodeT $ object [ "t" .=> "PostClaim"
                                                   , "pwd" .> pwd
                                                   , "cards" .> set
                                                   ])

    recv PostClaim{..} = do
        checkpwd pwd

        nboard <- uses cards length
        let dealCount = max 0 $ (boardSize (undefined :: card)) - (nboard - length i_cards)

        let tryDeal i = do
            newCards <- uses deck $ take dealCount
            let replaces = zip i_cards $ (Just <$> newCards) ++ repeat Nothing
            cs <- uses cards $ mapMaybe (\c -> fromMaybe (Just c) $ lookup c replaces)
            if nosets cs && i < 20
               then do
                   deck' <- join $ uses deck shuffle
                   deck .= deck'
                   tryDeal $ succ i
               else do
                   when (nosets cs) $ broadcast (Toast "failed to guarantee set existence")
                   now <- liftIO getCurrentTime
                   deck %= drop dealCount
                   cards .= cs
                   events <>= [(Dealt, newCards, now)]
                   broadcast $ Cards cs
                   return NewDesc

        tryDeal 0

        -- newCards <- deck %%= splitAt dealCount
        -- let replaces = zip i_cards $ (Just <$> newCards) ++ repeat Nothing
        -- cs <- cards <%= mapMaybe (\c -> fromMaybe (Just c) $ lookup c replaces)
        -- broadcast $ Cards cs
        -- return NewDesc

    recv PlusCard = do
        cs <- use cards

        if nosets cs
           then do
               newCard <- deck %%= splitAt 1
               if null newCard
                  then send $ Toast "no more sets!"
                  else do
                      now <- liftIO getCurrentTime
                      cs <- cards <<>= newCard
                      events <>= [(Added, newCard, now)]
                      broadcast $ Cards cs
           else send $ Toast "there are sets on the board!"

        return Done

    recv GetHistory = do
        hist <- use taken
        hist' <- mapM patchu hist
        send $ History hist'
        return Done
            where patchu h@(uid, _, _) = do
                    u <- view $ _2.byUid uid.uname
                    return $ h & _1 .~ u :: GameIO (SetVariantGame card) (Text, [card], UTCTime)
