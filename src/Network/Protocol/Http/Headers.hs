{-# LANGUAGE TypeOperators #-}
module Network.Protocol.Http.Headers where {- doc ok -}

import Control.Monad
import Data.Record.Label
import Misc.Text
import Network.Protocol.Http.Data
import Prelude hiding (lookup)
import Safe

{- | Access the /Content-Length/ header field. -}

contentLength :: (Read i, Integral i) => HTTP a :-> Maybe i
contentLength = (join . fmap readMay, fmap show) `lmap` header "Content-Length"

{- | Access the /Connection/ header field. -}

connection :: HTTP a :-> Maybe String
connection = header "Connection"

{- | Access the /Keep-Alive/ header field. -}

keepAlive :: (Read i, Integral i) => HTTP a :-> Maybe i
keepAlive = (join . fmap readMay, fmap show) `lmap` header "Keep-Alive"

{- | Access the /Cookie/ header field. -}

cookie :: HTTP Request :-> Maybe String
cookie = header "Cookie"

{- | Access the /Set-Cookie/ header field. -}

setCookie :: HTTP Response :-> Maybe String
setCookie = header "Set-Cookie"

{- | Access the /Location/ header field. -}

location :: HTTP a :-> Maybe String
location = header "Location"

{- |
Access the /Content-Type/ header field. The content-type will be parsed
into a mimetype and optional charset.
-}

contentType :: HTTP a :-> Maybe (String, Maybe String)
contentType = (join . fmap parser, fmap printer) `lmap` header "Content-Type"
  where parser a =
          case keyValues ";" "=" a of
            (m, Nothing):("charset", c):_ -> Just (m, c)
            _                             -> Nothing
        printer (x, y) = x ++ maybe "" (";charset="++) y

{- | Access the /Data/ header field. -}

date :: HTTP a :-> Maybe String
date = header "Date"

{- | Access the /Host/ header field. -}

hostname :: HTTP a :-> Maybe String
hostname = header "Host"

{- | Access the /Server/ header field. -}

server :: HTTP a :-> Maybe String
server = header "Server"

{- | Access the /User-Agent/ header field. -}

userAgent :: HTTP a :-> Maybe String
userAgent = header "User-Agent"

