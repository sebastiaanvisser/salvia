{-# LANGUAGE TemplateHaskell #-}
module Network.Protocol.Http.Data where

import Data.List (intercalate)
import Data.Map (lookup, insert, Map, empty)
import Data.Record.Label
import Data.List.Split
import Safe
import Misc.Text
import Network.Protocol.Http.Status (Status (..))
import Network.Protocol.Uri
import Prelude hiding (lookup)

{- | List of HTTP request methods. -}

data Method =
    OPTIONS
  | GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | TRACE
  | CONNECT
  | OTHER String
  deriving (Show, Eq)

{- | All recognized `Method` constructors as a list. -}

methods :: [Method]
methods = [OPTIONS, GET, HEAD, POST, PUT, DELETE, TRACE, CONNECT]

{- | HTTP protocol version. -}

data Version = Version {_major :: Int, _minor :: Int}
  deriving (Eq, Ord)

{- | Create HTTP 1.0 version. -}

http10 :: Version
http10 = Version 1 0

{- | Create HTTP 1.1 version. -}

http11 :: Version
http11 = Version 1 1

type HeaderKey   = String
type HeaderValue = String

{- | HTTP headers as mapping from keys to values. -}

type Headers     = Map HeaderKey HeaderValue

{- | Request or response specific part of HTTP messages. -}

data Direction =
    Request  { __method :: Method, __uri :: String }
  | Response { __status :: Status }
  deriving Eq

{- | An HTTP message. -}

data Message = Message
  { _direction :: Direction
  , _version   :: Version
  , _headers   :: Headers
  , _body      :: String
  } deriving Eq

{- | Create an empty HTTP request object. -}

emptyRequest :: Message
emptyRequest = Message (Request GET "") http11 empty ""

{- | Create an empty HTTP response object. -}

emptyResponse :: Message
emptyResponse = Message (Response OK) http11 empty ""

$(mkLabels [''Version, ''Direction, ''Message])

{- | Label to access the major part of the version. -}

major :: Label Version Int

{- | Label to access the minor part of the version. -}

minor :: Label Version Int

_status   :: Label Direction Status
_uri      :: Label Direction String
_method   :: Label Direction Method

{- | Label to access the body part of an HTTP message. -}

body :: Label Message String

{- | Label to access the header of an HTTP message. -}

headers :: Label Message Headers

{- | Label to access the version part of an HTTP message. -}

version :: Label Message Version

{- | Label to access the direction part of an HTTP message. -}

direction :: Label Message Direction

{- | Label to access the method part of an HTTP message. -}

method :: Label Message Method
method = _method % direction

{- | Label to access the URI part of an HTTP message. -}

uri :: Label Message String
uri = _uri % direction

asURI :: Label Message URI
asURI = (toURI, show) `lmap` uri

{- | Label to access the status part of an HTTP message. -}

status :: Label Message Status
status = _status % direction

{- | Normalize the capitalization of an HTTP header key. -}

normalizeHeader :: String -> String
normalizeHeader = intercalate "-" . map normalCase . splitOn "-"

{- | Generic label to access an HTTP header field by key. -}

header :: HeaderKey -> Label Message HeaderValue
header key = mkLabel
  (maybe "" id . lookup (normalizeHeader key) . lget headers)
  (lmod headers . insert (normalizeHeader key))

{- | Simply /utf-8/. -}

utf8 :: String
utf8 = "utf-8"

{- | Access the /Content-Length/ header field. -}

contentLength :: (Read i, Integral i) => Label Message (Maybe i)
contentLength = (readMay, maybe "" show) `lmap` header "Content-Length"

{- | Access the /Connection/ header field. -}

connection :: Label Message String
connection = header "Connection"

{- | Access the /Keep-Alive/ header field. -}

keepAlive :: (Read i, Integral i) => Label Message (Maybe i)
keepAlive = (readMay, maybe "" show) `lmap` header "Keep-Alive"

{- | Access the /Cookie/ and /Set-Cookie/ header fields. -}

cookie :: Label Message String
cookie = mkLabel (lget $ header "Cookie") (lset $ header "Set-Cookie")

{- | Access the /Location/ header field. -}

location :: Label Message String
location = header "Location"
--   (either (const Nothing) Just . parseURI . lget (header "Location"))
--   (lset (header "Location") . maybe "" show)

{- | Access the /Content-Type/ header field. -}

contentType :: Label Message (String, Maybe String)
contentType = (pa, pr) `lmap` header "Content-Type"
  where pr (t, c) = t ++ maybe "" ("; charset="++) c
        pa = error "no getter for contentType yet"

{- | Access the /Data/ header field. -}

date :: Label Message String
date = header "Date"

{- | Access the /Host/ header field. -}

hostname :: Label Message Authority
hostname = mkLabel
  (either (const mkAuthority) id . parseAuthority . lget (header "Host"))
  (lset (header "Host") . show)

{- | Access the /Server/ header field. -}

server :: Label Message String
server = header "Server"

userAgent :: Label Message String
userAgent = header "User-Agent"

