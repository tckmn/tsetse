module Util
    ( shuffle
    , makeSecret
    , encodeT, decodeT
    , (.==.), (.&&.), (.$.)
    , (!!!)
    , Diffable, diffable, diff, linear
    , folds
    ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.List (sortOn, permutations)
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
(.&&.) :: Applicative f => f Bool -> f Bool -> f Bool
(.&&.) = liftA2 (&&)
(.$.) :: Applicative f => f (a -> b) -> f a -> f b
(.$.) = liftA2 ($)

(!!!) :: [a] -> Int -> Maybe a
[] !!! _ = Nothing
(x:xs) !!! 0 = Just x
(x:xs) !!! i = xs !!! pred i

diffable :: Eq b => (a -> a -> b) -> a -> a -> a -> a -> Bool
diffable f a b c d = f a b == f c d
class Diffable a where
    diff :: a -> a -> a -> a -> Bool

linear :: Diffable a => [a] -> Bool
linear = any linear' . permutations
    where linear' [a,b,c] = diff a b b c
          linear' _ = False

notouch :: [(Int, Bool)] -> Bool
notouch ((n1,a1):c@(n2,a2):cs) = n1 /= n2 && notouch (c:cs)
notouch _ = True

foldable' :: Int -> [(Int, Bool)] -> Bool
foldable' paper [(n1,a1),(n2,a2)] = n2 - n1 == paper `div` 2 && a1 == a2
foldable' paper cs = or [ and [ ass i /= ass (i+1)
                              , dist (i-1) >= dist i
                              , dist (i+1) >= dist i
                              , foldable' (paper - 2*dist i) (crimp i)
                              ]
                        | i <- [0..length cs - 1]
                        ]
    where num i = fst $ cs !! (i `mod` length cs)
          ass i = snd $ cs !! (i `mod` length cs)
          dist i = (num (i+1) - num i) `mod` paper
          crimp i = [(n',a) | (n,a) <- cs,
                              n /= num i && n /= num (i+1),
                              let n' = if n > num (i+1) then n - 2*dist i else n
                    ]

folds :: Int -> [(Int, Bool)] -> Bool
folds paper = (notouch .&&. foldable' paper) . sortOn fst
