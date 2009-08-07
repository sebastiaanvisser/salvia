module Network.Protocol.Uri.Chars
  ( unreserved
  , genDelims
  , subDelims
  ) where

import Data.Char

-- 2.3.  Unreserved Characters
unreserved :: Char -> Bool
unreserved c = isAlphaNum c || elem c "-._~"

-- 2.2.  Reserved Characters
genDelims :: Char -> Bool
genDelims = flip elem ":/?#[]@"

subDelims :: Char -> Bool
subDelims = flip elem "!$&'()*+,;="

