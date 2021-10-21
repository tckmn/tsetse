module Util
    ( shuffle
    , makeSecret
    , encodeT, decodeT
    ) where

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import System.Random
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle [] g = ([], g)
shuffle xs g = let (idx, g') = randomR (0, length xs - 1) g
                   (left, (x:right)) = splitAt idx xs
                   (xs', g'') = shuffle (left++right) g'
                in (x:xs', g'')

secretChars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

makeSecret :: IO Text
makeSecret = fmap T.pack . replicateM 32 $ (secretChars !!) <$> randomRIO (0, length secretChars - 1)

encodeT :: ToJSON a => a -> Text
encodeT = T.decodeUtf8 . LB.toStrict . encode
decodeT :: FromJSON a => Text -> Maybe a
decodeT = decode . LB.fromStrict . T.encodeUtf8
