module Network.Protocol.Http.Parser {- doc ok -}
  (

  -- * Top level message parsers.

    parseRequest
  , parseResponse

  -- * Exposure of internal parsec parsers.

  , pRequest
  , pResponse
  , pHeaders
  , pVersion
  , pMethod

  -- * Helper labels for accessing lists and maps encoded in strings.

  , keyValues
  , values

  ) where

import Control.Applicative hiding (empty)
import Control.Monad
import Data.Char
import Data.List (intercalate)
import Data.List.Split hiding (oneOf)
import Data.Map (insert, empty)
import Data.Record.Label
import Misc.Text
import Network.Protocol.Http.Data
import Network.Protocol.Http.Status
import Text.Parsec hiding (many, (<|>))
import Text.Parsec.Prim (Stream, ParsecT)

-- | Parse a string as an HTTP request message. This parser is very forgiving.

parseRequest :: String -> Either ParseError (Http Request)
parseRequest = parse pRequest ""

-- | Parse a string as an HTTP request message. This parser is very forgiving.

parseResponse :: String -> Either ParseError (Http Response)
parseResponse = parse pResponse ""

-- | Parsec parser to parse the header part of an HTTP request.

pRequest :: Stream s m Char => ParsecT s u m (Http Request)
pRequest =
      (\m u v h -> Http (Request m u) v h)
  <$> (pMethod <* many1 (oneOf ls))
  <*> (many1 (noneOf ws) <* many1 (oneOf ls))
  <*> (pVersion <* eol)
  <*> (pHeaders <* eol)

-- | Parsec parser to parse the header part of an HTTP response.

pResponse :: Stream s m Char => ParsecT s u m (Http Response)
pResponse =
      (\v s h -> Http (Response (statusFromCode $ read s)) v h)
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
  <$> (istring "HTTP/" *> digit)
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
pMaybe a = option Nothing (Just <$> a)

-- Parse end of line, \r, \n or \r\n.

eol :: Stream s m Char => ParsecT s u m ()
eol = () <$ ((char '\r' <* pMaybe (char '\n')) <|> char '\n')

-- Case insensitive string parser.

istring :: Stream s m Char => String -> ParsecT s u m String
istring s = sequence (map (\c -> satisfy (\d -> toUpper c == toUpper d)) s)

-- | Label for accessing key value pairs encoded in a string.

keyValues :: String -> String -> String :-> [(String, Maybe String)]
keyValues sep eqs = mkLabel parser (\a _ -> printer a)
  where parser =
            filter (\(a, b) -> not (null a) || b /= Nothing && b /= Just "")
          . map (f . splitOn eqs)
          . concat
          . map (splitOn sep)
          . lines
          where f []     = ("", Nothing)
                f [x]    = (trim x, Nothing)
                f (x:xs) = (trim x, Just . trim $ intercalate eqs xs)
        printer = intercalate sep . map (\(a, b) -> a ++ maybe "" (eqs ++) b)

-- | Label for accessing lists of values encoded in a string.

values :: String -> String :-> [String]
values sep = mkLabel parser (\a _ -> printer a)
  where parser = filter (not . null) . concat . map (splitOn sep) . lines
        printer = intercalate sep

