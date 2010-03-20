{-# LANGUAGE FlexibleContexts, TypeOperators #-}
module Network.Salvia.Handler.Range
( Range (..)
, contentRange
, range
, rangeL
)
where

import Data.Record.Label
import Data.List
import Network.Protocol.Http
import Safe

-- | HTTP Range datatype.

data Range = Range (Maybe Integer) (Maybe Integer) (Maybe Integer)
  deriving Show

-- | Access the /Content-Range/ header field.

contentRange :: Http Response :-> Maybe Range
contentRange = lmap rangeL `iso` header "Content-Range"

-- | Access the /Range/ header field.

range :: Http Request :-> Maybe Range
range = lmap rangeL `iso` header "Range"

-- | Lens containing parser and pretty-printer for HTTP ranges.

rangeL :: String :<->: Range
rangeL = parser <-> printer
  where
  printer (Range f t x) = concat ["bytes ", maybe "" show f, "-", maybe "" show t, maybe "" (('/':).show) x]
  parser r =
    case span (/='-') . maybe r id $ stripPrefix "bytes=" r of
      (f, _:a) -> case span (/='/') a of
                    (t, _:x) -> Range (readMay f) (readMay t) (readMay x)
                    (t, _)   -> Range (readMay f) (readMay t) Nothing
      (f, [])  ->               Range (readMay f) Nothing Nothing

-- If-Range: "ac5319-31f76000-481fa6ec42cc0"
-- Range: bytes=125817136-
-- Content-Range:  "bytes 21010-47021/47022"
-- "ac5319-31f76000-481fa6ec42cc0"
-- Last-Modified: Wed, 17 Mar 2010 07:55:07 GMT
-- : bytes

