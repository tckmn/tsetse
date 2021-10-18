module Util where

import System.Random

shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle [] g = ([], g)
shuffle xs g = let (idx, g') = randomR (0, length xs - 1) g
                   (left, (x:right)) = splitAt idx xs
                   (xs', g'') = shuffle (left++right) g'
                in (x:xs', g'')
