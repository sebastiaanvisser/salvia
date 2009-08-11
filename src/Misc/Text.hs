module Misc.Text where

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

-- | Start with capital, tail is lower.
normalCase :: String -> String
normalCase ""     = ""
normalCase (x:xs) = toUpper x : map toLower xs

-- | Trim all heading and trailing whitespace.
trim :: String -> String
trim = rev (dropWhile (`elem` " \t\n\r"))
  where rev f = reverse . f . reverse . f

-- | Parse key value pairs.
keyValues :: String -> String -> String -> [(String, Maybe String)]
keyValues sep eqs =
    filter (\(a, b) -> not (null a) || b /= Nothing && b /= Just "")
  . map (f . splitOn eqs)
  . concat
  . map (splitOn sep)
  . lines
  where f []     = ("", Nothing)
        f [x]    = (trim x, Nothing)
        f (x:xs) = (trim x, Just . trim $ intercalate eqs xs)

-- | ShowS version of intersperse.

intersperseS :: ShowS -> [ShowS] -> ShowS
intersperseS _ []  = id
intersperseS _ [s] = s
intersperseS c xs  = foldl1 (\a -> ((a.c).)) xs


