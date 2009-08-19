module Network.Protocol.Uri.Query where

import Data.Record.Label
import Control.Applicative hiding (empty)
import Network.Protocol.Uri.Data
import Text.Parsec hiding (many, (<|>))
import Text.Parsec.Prim (Stream, ParsecT)

type Parameters = [(String, Maybe String)]

pQueryParams :: Stream s m Char => ParsecT s u m Parameters
pQueryParams = 
      filter (not . null . fst)
  <$> sepBy pParam (char '&')

pParam :: Stream s m Char => ParsecT s u m (String, Maybe String)
pParam = (,)
  <$> many (noneOf "=&")
  <*> pMaybe (char '=' *> (translateParam <$> many (noneOf "&")))
      
pMaybe :: Stream s m t => ParsecT s u m a -> ParsecT s u m (Maybe a)
pMaybe a = option Nothing (Just <$> a)

{- | Parse a pre-decoded query string into key value pairs parameters. -}

parseQueryParams :: String -> Either ParseError Parameters
parseQueryParams = parse pQueryParams  ""

{- | Fetch the query parameters form a URI. -}

queryParams :: URI -> Parameters
queryParams = either (const []) id . parseQueryParams . lget query

-- Translate special characters in a parameter.
translateParam :: String -> String
translateParam []       = []
translateParam ('+':xs) = ' ' : translateParam xs
translateParam (x:xs)   = x   : translateParam xs

