{-# LANGUAGE FlexibleContexts #-}
module Network.Protocol.Http.Parser {- doc ok -}

  -- * Top level message parsers.
  ( parseRequest
  , parseResponse

  -- * Exposure of internal parsec parsers.
  , pRequest
  , pResponse
  , pHeaders
  , pVersion
  , pMethod

  ) where

import Control.Applicative hiding (empty)
import Control.Monad
import Data.Char
import Data.List (intercalate)
import Data.Map (insert, empty)
import Network.Protocol.Http.Data
import Network.Protocol.Http.Status
import Text.Parsec hiding (many, (<|>))
import Text.Parsec.Prim (Stream, ParsecT)

-- | Parse a string as an HTTP request message. This parser is very forgiving.

parseRequest :: String -> Either ParseError (HTTP Request)
parseRequest = parse pRequest ""

-- | Parse a string as an HTTP request message. This parser is very forgiving.

parseResponse :: String -> Either ParseError (HTTP Response)
parseResponse = parse pResponse ""

-- | Parsec parser to parse the header part of an HTTP request.

pRequest :: Stream s m Char => ParsecT s u m (HTTP Request)
pRequest =
      (\m u v h -> HTTP (Request m u) v h)
  <$> (pMethod <* many1 (oneOf ls))
  <*> (many1 (noneOf ws) <* many1 (oneOf ls))
  <*> (pVersion <* eol)
  <*> (pHeaders <* eol)

-- | Parsec parser to parse the header part of an HTTP response.

pResponse :: Stream s m Char => ParsecT s u m (HTTP Response)
pResponse =
      (\v s h -> HTTP (Response (statusFromCode $ read s)) v h)
  <$> (pVersion <* many1 (oneOf ls))
  <*> (many1 digit <* many1 (oneOf ls) <* many1 (noneOf lf) <* eol)
  <*> (pHeaders <* eol)

-- | Parsec parser to parse one or more, possibly multiline, HTTP header lines.

pHeaders :: Stream s m Char => ParsecT s u m Headers
pHeaders = Headers <$> p
  where
    p = insert
        <$> many1 (noneOf (':':ws)) <* string ":"
        <*> (intercalate ws <$> (many $ many1 (oneOf ls) *> many1 (noneOf lf) <* eol))
        <*> option empty p

-- | Parsec parser to parse HTTP versions. Recognizes X.X versions only.

pVersion :: Stream s m Char => ParsecT s u m Version
pVersion = 
      (\h l -> Version (ord h - ord '0') (ord l  - ord '0'))
  <$> (string "HTTP/" *> digit)
  <*> (char '.'       *> digit)

-- | Parsec parser to parse an HTTP method. Parses arbitrary method but
-- actually recognizes the ones listed as a constructor for `Method'.

pMethod :: Stream s m Char => ParsecT s u m Method
pMethod =
     choice
   $ map (\a -> a <$ (try . istring . show $ a)) methods
  ++ [OTHER <$> many (noneOf ws)]

-- Helpers.

lf, ws, ls :: String
lf = "\r\n"
ws = " \t\r\n"
ls = " \t"

-- Optional parser with maybe result.

pMaybe :: Stream s m t => ParsecT s u m a -> ParsecT s u m (Maybe a)
pMaybe a = option Nothing . liftM Just $ a

-- Parse end of line, \r, \n or \r\n.

eol :: Stream s m Char => ParsecT s u m ()
eol = () <$ ((char '\r' <* pMaybe (char '\n')) <|> char '\n')

-- Case insensitive string parser.

istring :: Stream s m Char => String -> ParsecT s u m String
istring s = sequence (map (\c -> satisfy (\d -> toUpper c == toUpper d)) s)

