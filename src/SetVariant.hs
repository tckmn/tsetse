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
import Types hiding ((.=))
import Util

import Data.Aeson

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

data SetVariantGame card =
    SetVariantGame { _deck :: [card]
                   , _cards :: [card]
                   , _taken :: [(ClientId, [card], UTCTime)]
                   , _rudeness :: HashMap ClientId [UTCTime]
                   }

instance Binary card => Binary (SetVariantGame card) where
    put SetVariantGame{..} = do B.put _deck
                                B.put _cards
                                B.put _taken
    get = do SetVariantGame <$> B.get
                            <*> B.get
                            <*> B.get
                            <*> pure M.empty

makeLenses ''SetVariantGame

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
    toJSON Cards{..} = object ["t" .= ("Cards" :: Text), "cards" .= o_cards]
    toJSON UserInfo{..} = object ["t" .= ("UserInfo" :: Text), "score" .= o_score]
    toJSON History{..} = object ["t" .= ("History" :: Text), "history" .= o_history]

instance (Binary card, SetVariant card) => Game (SetVariantGame card) (Msg card) where

    new = do
        shuf <- shuffle fullDeck
        let (cards, deck) = splitAt (boardSize (undefined :: card)) shuf
        return SetVariantGame { _deck = deck
                              , _cards = cards
                              , _taken = []
                              , _rudeness = M.empty
                              }

    catchup = do
        cs <- use cards
        send $ Cards cs

    players g = nub $ g^..taken.folded._1

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
        taken <>= [(who, set, when)]
        broadcast $ Highlight idxs' True
        userlist

        -- wait 5 seconds and clear the cards
        pwd <- view $ _2.password
        return $ Delayed 5000000 (encodeT $ object [ "t" .= ("PostClaim" :: Text)
                                                   , "pwd" .= pwd
                                                   , "cards" .= set
                                                   ])

    recv PostClaim{..} = do
        checkpwd pwd

        nboard <- uses cards length
        let dealCount = max 0 $ (boardSize (undefined :: card)) - (nboard - length i_cards)

        -- oh my god what a beautiful line
        newCards <- deck %%= splitAt dealCount
        let replaces = zip i_cards $ (Just <$> newCards) ++ repeat Nothing
        cs <- cards <%= mapMaybe (\c -> fromMaybe (Just c) $ lookup c replaces)
        broadcast $ Cards cs
        return NewDesc

    recv PlusCard = do
        subs <- uses cards subsequences
        let sets = [s | s <- subs, length s `elem` setSizes (undefined :: card), checkSet s]

        if null sets
           then do
               newCard <- deck %%= splitAt 1
               cs <- cards <<>= newCard
               broadcast $ Cards cs
           else do
               liftIO . putStrLn . T.unpack . encodeT $ sets
               send $ Toast "there are sets on the board!"

        return Done

    recv GetHistory = do
        hist <- use taken
        hist' <- mapM patchu (reverse hist)
        send $ History hist'
        return Done
            where patchu h@(uid, _, _) = do
                    u <- view $ _2.byUid uid.uname
                    return $ h & _1 .~ u :: GameIO (SetVariantGame card) (Text, [card], UTCTime)
