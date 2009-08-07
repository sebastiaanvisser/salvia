{- |
For more information: http://www.ietf.org/rfc/rfc2109.txt
-}

module Network.Protocol.Cookie (
    Cookie (..)
  , empty

  , Cookies
  , cookies
  , cookie
  , showCookies

  , parseCookies
  ) where

import Control.Applicative hiding (empty)
import Control.Monad ()
import Data.Char (toLower)
import Data.List (intercalate)
import Misc.Misc (intersperseS, (@@), trim)
import Network.Protocol.Uri (URI)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import qualified Data.Map as M


{- |
The `Cookie` data type containg one key/value pair with all the (potentially
optional) meta-data.
-}

data Cookie =
  Cookie {
    name       :: String
  , value      :: String
  , comment    :: Maybe String
  , commentURL :: Maybe URI
  , discard    :: Bool
  , domain     :: Maybe String
  , maxAge     :: Maybe Int
  , expires    :: Maybe String
  , path       :: Maybe String
  , port       :: [Int]
  , secure     :: Bool
  , version    :: Int
  }

{- | Create an empty cookie. -}

empty :: Cookie
empty = Cookie {
    name       = ""
  , value      = ""
  , comment    = Nothing
  , commentURL = Nothing
  , discard    = False
  , domain     = Nothing
  , maxAge     = Nothing
  , expires    = Nothing
  , path       = Nothing
  , port       = []
  , secure     = False
  , version    = 0
  }

{- |
A collection of multiple cookies. These can all be set in one single HTTP
/Set-Cookie/ header field.
-}

type Cookies = M.Map String Cookie

{- |
Convert a list of cookies into a cookie mapping. The name will be used as the
key, the cookie itself as the value.
-}

cookies :: [Cookie] -> Cookies
cookies = M.fromList . map (\a -> (name a, a))

{- | Case-insensitive way of getting a cookie out of a collection by name. -}

cookie :: String -> Cookies -> Maybe Cookie
cookie n = M.lookup (map toLower n)

-------[ cookie show instance ]------------------------------------------------

instance Show Cookie where
  show = flip showsCookie ""

-- Show a semicolon separated list of attribute/value pairs. Only meta pairs
-- with significant values will be pretty printed.
showsCookie :: Cookie -> ShowS
showsCookie c =
    pair     (name c)     (value c)
  . opt      "comment"    (comment c)
  . opt      "commentURL" (fmap show $ commentURL c)
  . bool     "discard"    (discard c)
  . opt      "domain"     (domain c)
  . opt      "maxAge"     (fmap show $ maxAge c)
  . opt      "expires"    (expires c)
  . opt      "path"       (path c)
  . list     "port"       (map show $ port c)
  . bool     "secure"     (secure c)
  . opt      "version"    (optval $ version c)
  where
    attr a       = showString a
    val v        = showString ("=" ++ v)
    end          = showString "; "
    single a     = attr a . end
    pair a v     = attr a . val v . end
    opt a        = maybe id (pair a)
    list _ []    = id
    list a xs    = pair a $ intercalate "," xs
    bool _ False = id
    bool a True  = single a
    optval 0     = Nothing
    optval i     = Just (show i)

{- | Show multiple cookies, pretty printed using a comma separator. -}

showCookies :: Cookies -> String
showCookies = ($"")
  . intersperseS (showString ", ")
  . map (shows . snd)
  . M.toList

-------[ cookie parser ]-------------------------------------------------------

{- |
Parse a set of cookie values and turn this into a collection of real cookies.
As the specification states, only the name, value, domain, path and port will
be recognized.
-}

parseCookies :: String -> Maybe Cookies
parseCookies = fmap cookies . (pCookie @@)

pCookie :: GenParser Char st [Cookie]
pCookie = map ck <$> pCookieValues
  where
    ck ((n, v), m) =
      empty {
        name   = n
      , value  = v
      , domain = "$domain" `M.lookup` m
      , path   = "$path"   `M.lookup` m
      , port   = maybe [] pPorts $
                 "$port"   `M.lookup` m
      }

-- Parse a list of comma separated portnumbers. Returns a list of integers or
-- an empty list on failure.
pPorts :: String -> [Int]
pPorts = either (const []) id . parse p ""
   where
    p = char '"'
     *> sepBy1 (read <$> many1 digit) (char ',')
     <* char '"'

-- Parse a collection of cookie name/value pairs.
pCookieValues :: GenParser Char st [((String, String), M.Map String String)]
pCookieValues =
      flip sepBy1 sep ((,)
  <$> pair key
  <*> (M.fromList
  <$> filter (not . null . fst)
  <$> many (try (sep *> pair skey))))
  where
    f      = trim . map toLower
    key    = f <$> many1 (noneOf "=;")
    skey   = f <$> ((:) <$> char '$' <*> many (noneOf "=;"))
    val    = trim <$> many (noneOf ";")
    pair k = (,) <$> k <*> (char '=' *> val)
    sep    = char ';' *> many (oneOf " \t\r\n")

