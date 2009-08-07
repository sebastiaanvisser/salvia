{-# LANGUAGE FlexibleContexts #-}
module Network.Protocol.Http.Parser (

  -- * Top level message parsers.
    parseRequest
  , parseResponse

  -- * Exposure of internal parsec parsers.
  , pRequest
  , pResponse
  , pVersion
  , pHeaders
  , pMethod

  ) where

import Control.Applicative hiding (empty)
import Control.Monad
import Data.Char
import Data.List (intercalate)
import Data.Map (insert, empty)
import Network.Protocol.Http.Data
import Network.Protocol.Http.Status
import Network.Protocol.Uri
import Text.Parsec hiding (many, (<|>))
import Text.Parsec.Prim (Stream, ParsecT)

{- | Parse a string as an HTTP request message. -}

parseRequest :: String -> Either ParseError Message
parseRequest = parse pRequest ""

{- | Parse a string as an HTTP request message. -}

parseResponse :: String -> Either ParseError Message
parseResponse = parse pResponse ""

{- | Parsec parser to parse HTTP versions. Recognizes X.X versions only. -}

pVersion :: Stream s m Char => ParsecT s u m Version
pVersion = 
      (\h l -> Version (ord h - ord '0') (ord l  - ord '0'))
  <$> (string "HTTP/" *> digit)
  <*> (char '.'       *> digit)

{- |
Parsec parser to parse an HTTP method. Parses arbitrary method but actually
recognizes the ones listed as a constructor for `Method'.
-}

pMethod :: Stream s m Char => ParsecT s u m Method
pMethod =
     choice
   $ map (\a -> a <$ (try . istring . show $ a)) methods
  ++ [OTHER <$> many (noneOf ws)]

{- |
Parsec parser to parse one or more, possibly multiline, HTTP header lines.
-}

pHeaders :: Stream s m Char => ParsecT s u m Headers
pHeaders =
      insert
  <$> many1 (noneOf (':':ws)) <* string ":"
  <*> (intercalate ws <$> (many $ many1 (oneOf ls) *> many1 (noneOf lf) <* eol))
  <*> option empty pHeaders

{- | Parsec parser to parse an HTTP request.-}

pRequest :: Stream s m Char => ParsecT s u m Message
pRequest =
      (\m u v h b -> Message (Request m u) v h b)
  <$> (pMethod <* many1 (oneOf ls))
  <*> (pUriReference <* many1 (oneOf ls))
  <*> (pVersion <* eol)
  <*> (pHeaders <* eol)
  <*> many anyToken

{- | Parsec parser to parse an HTTP response. -}

pResponse :: Stream s m Char => ParsecT s u m Message
pResponse =
      (\v s h b -> Message (Response (statusFromCode $ read s)) v h b)
  <$> (pVersion <* many1 (oneOf ls))
  <*> (many1 digit <* many1 (oneOf ls) <* many1 (noneOf lf) <* eol)
  <*> (pHeaders <* eol)
  <*> many anyToken

-- Helpers.

lf, ws, ls :: String
lf = "\r\n"
ws = " \t\r\n"
ls = " \t"

-- Option parser with maybe result.

pMaybe :: Stream s m t => ParsecT s u m a -> ParsecT s u m (Maybe a)
pMaybe a = option Nothing . liftM Just $ a

-- Parse end of line, \r, \n or \r\n.

eol :: Stream s m Char => ParsecT s u m ()
eol = () <$ ((char '\r' <* pMaybe (char '\n')) <|> char '\n')

-- Case insensitive string parser.

istring :: Stream s m Char => String -> ParsecT s u m String
istring s = sequence (map (\c -> satisfy (\d -> toUpper c == toUpper d)) s)

