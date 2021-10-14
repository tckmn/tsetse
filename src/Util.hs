module Util where

import Data.Char

capitalize :: String -> String
capitalize "" = ""
capitalize (c:s) = toUpper c : s
