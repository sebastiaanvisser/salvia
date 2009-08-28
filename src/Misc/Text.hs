module Misc.Text where

import Data.Char

-- | Start with capital, tail is lower.
normalCase :: String -> String
normalCase ""     = ""
normalCase (x:xs) = toUpper x : map toLower xs

-- | Trim all heading and trailing whitespace.
trim :: String -> String
trim = rev (dropWhile (`elem` " \t\n\r"))
  where rev f = reverse . f . reverse . f

-- | ShowS version of intersperse.

intersperseS :: ShowS -> [ShowS] -> ShowS
intersperseS _ []  = id
intersperseS _ [s] = s
intersperseS c xs  = foldl1 (\a -> ((a.c).)) xs

