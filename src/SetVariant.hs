{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SetVariant where

import Control.Applicative
import Data.List (nub)
import Data.Maybe
import GHC.Generics
import qualified Data.HashMap.Strict as M

import GameUtil
import GM
import Types hiding ((.=))
import Util

import Data.Aeson

class (Eq card, ToJSON card, FromJSON card) => SetVariant card where
    name :: card -> Text
    boardSize :: card -> Int
    fullDeck :: [card]
    checkSet :: [card] -> Bool

data SetVariantGame card =
    SetVariantGame { _deck :: [card]
                   , _cards :: [card]
                   , _taken :: [card]
                   , _scores :: M.HashMap ClientId Int
                   }
makeLenses ''SetVariantGame

data Msg card = Claim { idxs :: [Int] }
              | PostClaim { pwd :: Text, i_cards :: [card] }

instance SetVariant card => FromJSON (Msg card) where
    parseJSON (Object v) = do
        t <- v .: "t"
        case t of
          String "Claim" -> Claim <$> v .: "idxs"
          String "PostClaim" -> PostClaim <$> v .: "pwd" <*> v .: "cards"
          _ -> empty
    parseJSON _ = empty

data OutMsg card = Cards { o_cards :: [card] }
                 | UserInfo { o_score :: Int }

instance SetVariant card => ToJSON (OutMsg card) where
    toJSON Cards{..} = object ["t" .= ("Cards" :: Text), "cards" .= o_cards]
    toJSON UserInfo{..} = object ["t" .= ("UserInfo" :: Text), "score" .= o_score]

instance SetVariant card => Game (SetVariantGame card) (Msg card) where

    new = do
        shuf <- shuffle fullDeck
        let (cards, deck) = splitAt (boardSize (undefined :: card)) shuf
        return SetVariantGame { _deck = deck
                              , _cards = cards
                              , _taken = []
                              , _scores = M.empty
                              }

    catchup = do
        cs <- use cards
        send $ Cards cs

    players g = g^.scores&M.keys

    userinfo g cid = toJSON (UserInfo (g^.scores.at cid.non 0) :: OutMsg card)

    desc g = (name (undefined :: card), "whee")

    recv Claim{..} = do
        -- make sure the request is well-formed
        cs <- use cards
        ts <- use taken
        let idxs' = nub idxs
            set = [c | idx <- idxs', let Just c = cs^?ix idx, c `notElem` ts]
        guard $ length idxs' == length set

        -- flash em red if they're not a set
        unless (checkSet set) $ do
            send $ Highlight idxs' False
            empty

        -- they're a set! tell everyone
        taken <>= set
        broadcast $ Highlight idxs' True

        -- gain some score
        who <- view $ _1.cid
        scores.at who %= Just . (+length set) . fromMaybe 0
        userlist

        -- wait 5 seconds and clear the cards
        pwd <- view $ _2.password
        return $ Delayed 5000000 (encodeT $ object [ "t" .= ("PostClaim" :: Text)
                                                   , "pwd" .= pwd
                                                   , "cards" .= set
                                                   ])

    recv PostClaim{..} = do
        checkpwd pwd

        -- oh my god what a beautiful line
        newCards <- deck %%= splitAt (length i_cards)
        let replaces = zip i_cards $ (Just <$> newCards) ++ repeat Nothing
        cs <- cards <%= mapMaybe (\c -> fromMaybe (Just c) $ lookup c replaces)
        broadcast $ Cards cs
        return Done
