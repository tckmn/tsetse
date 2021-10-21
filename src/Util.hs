module Util
    ( shuffle
    , makeSecret
    , encodeT, decodeT
    , (.==.)
    ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Text (Text)
import System.Random
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    idx <- randomRIO (0, length xs - 1)
    let (left, (x:right)) = splitAt idx xs
    (x:) <$> shuffle (left++right)

secretChars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

makeSecret :: IO Text
makeSecret = fmap T.pack . replicateM 32 $ (secretChars !!) <$> randomRIO (0, length secretChars - 1)

encodeT :: ToJSON a => a -> Text
encodeT = T.decodeUtf8 . LB.toStrict . encode
decodeT :: FromJSON a => Text -> Maybe a
decodeT = decode . LB.fromStrict . T.encodeUtf8

(.==.) :: (Eq a, Applicative f) => f a -> f a -> f Bool
(.==.) = liftA2 (==)
