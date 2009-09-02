{-# LANGUAGE TemplateHaskell #-}
-- | For more information: http://www.ietf.org/rfc/rfc2109.txt
module Network.Protocol.Cookie {- todo: test please -}
  (
  
  -- * Cookie datatype.
    Cookie (Cookie)
  , cookie
  , empty

  -- * Accessing cookies.
  , name
  , value
  , comment
  , commentURL
  , discard
  , domain
  , maxAge
  , expires
  , path
  , port
  , secure
  , version

  -- * Collection of cookies.

  , Cookies (..)
  , cookies
  , getCookie

  , fromList
  , toList

  )
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad (join)
import Data.Record.Label
import Data.Maybe
import Data.Char
import Safe
import Data.List
import Misc.Text
import Network.Protocol.Uri.Query
import qualified Data.Map as M

-- | The `Cookie` data type containg one key/value pair with all the
-- (potentially optional) meta-data.

data Cookie =
  Cookie
    { _name       :: String
    , _value      :: String
    , _comment    :: Maybe String
    , _commentURL :: Maybe String
    , _discard    :: Bool
    , _domain     :: Maybe String
    , _maxAge     :: Maybe Int
    , _expires    :: Maybe String
    , _path       :: Maybe String
    , _port       :: [Int]
    , _secure     :: Bool
    , _version    :: Int
    } deriving Eq

$(mkLabels [''Cookie])

-- | Access name/key of a cookie.

name :: Cookie :-> String

-- | Access value of a cookie.

value :: Cookie :-> String

-- | Access comment of a cookie.

comment :: Cookie :-> Maybe String

-- | Access comment-URL of a cookie.

commentURL :: Cookie :-> Maybe String

-- | Access discard flag of a cookie.

discard :: Cookie :-> Bool

-- | Access domain of a cookie.

domain :: Cookie :-> Maybe String

-- | Access max-age of a cookie.

maxAge :: Cookie :-> Maybe Int

-- | Access expiration of a cookie.

expires :: Cookie :-> Maybe String

-- | Access path of a cookie.

path :: Cookie :-> Maybe String

-- | Access port of a cookie.

port :: Cookie :-> [Int]

-- | Access secure flag of a cookie.

secure :: Cookie :-> Bool

-- | Access version of a cookie.

version :: Cookie :-> Int

-- | Create an empty cookie.

empty :: Cookie
empty = Cookie "" "" Nothing Nothing False Nothing Nothing Nothing Nothing [] False 0

-- Cookie show instance.

instance Show Cookie where
  showsPrec _ = showsCookie

-- Show a semicolon separated list of attribute/value pairs. Only meta pairs
-- with significant values will be pretty printed.

showsCookie :: Cookie -> ShowS
showsCookie c =
    pair (get name c) (get value c)
  . opt  "comment"    (get comment c)
  . opt  "commentURL" (get commentURL c)
  . bool "discard"    (get discard c)
  . opt  "domain"     (get domain c)
  . opt  "maxAge"     (fmap show $ get maxAge c)
  . opt  "expires"    (get expires c)
  . opt  "path"       (get path c)
  . lst  "port"       (map show $ get port c)
  . bool "secure"     (get secure c)
  . opt  "version"    (optval $ get version c)
  where
    attr a       = showString a
    val v        = showString ("=" ++ v)
    end          = showString "; "
    single a     = attr a . end
    pair a v     = attr a . val v . end
    opt a        = maybe id (pair a)
    lst _ []     = id
    lst a xs     = pair a $ intercalate "," xs
    bool _ False = id
    bool a True  = single a
    optval 0     = Nothing
    optval i     = Just (show i)

parseCookie :: String -> Cookie
parseCookie s = 
  let p = forth (keyValues ";" "=") s
  in Cookie
    { _name       = (fromMaybe "" .        fmap fst . headMay)              p
    , _value      = (fromMaybe "" . join . fmap snd . headMay)              p
    , _comment    = (                           join . lookup "comment")    p
    , _commentURL = (                           join . lookup "commentURL") p
    , _discard    = (maybe False (const True) . join . lookup "discard")    p
    , _domain     = (                           join . lookup "commentURL") p
    , _maxAge     = (join . fmap readMay .      join . lookup "commentURL") p
    , _expires    = (                           join . lookup "expires")    p
    , _path       = (                           join . lookup "path")       p
    , _port       = (maybe [] (readDef [-1]) .  join . lookup "port")       p
    , _secure     = (maybe False (const True) . join . lookup "secure")     p
    , _version    = (maybe 1 (readDef 1) .      join . lookup "version")    p
    }

-- | Cookie parser and pretty printer as a lens.

cookie :: Lens Cookie String
cookie = show <-> parseCookie

-- | A collection of multiple cookies. These can all be set in one single HTTP
-- /Set-Cookie/ header field.

newtype Cookies = Cookies { unCookies :: M.Map String Cookie }
  deriving Eq

instance Show Cookies where
  showsPrec _ =
      intersperseS (showString ", ")
    . map (shows . snd)
    . M.toList
    . unCookies

-- | Cookies parser and pretty printer as a lens.

cookies :: Lens String Cookies
cookies = (fromList . map parseCookie <-> map show . toList) . values ","

-- | Case-insensitive way of getting a cookie out of a collection by name.

getCookie :: String -> Cookies -> Maybe Cookie
getCookie n = M.lookup (map toLower n) . unCookies

-- | Convert a list to a cookies collection.

fromList :: [Cookie] -> Cookies
fromList = Cookies . M.fromList . map (\a -> (get name a, a))

-- | Get the cookies as a list.

toList :: Cookies -> [Cookie]
toList = map snd . M.toList . unCookies

