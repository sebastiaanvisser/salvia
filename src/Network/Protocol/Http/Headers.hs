module Network.Protocol.Http.Headers where {- doc ok -}

import Control.Category
import Control.Monad
import Data.Record.Label
import Network.Protocol.Http.Data
import Network.Protocol.Uri.Query
import Prelude hiding ((.), id)
import Safe

-- | Access the /Content-Length/ header field.

contentLength :: (Read i, Integral i) => Http a :-> Maybe i
contentLength = (join . fmap readMay <-> fmap show) `iso` header "Content-Length"

-- | Access the /Connection/ header field.

connection :: Http a :-> Maybe String
connection = header "Connection"

-- | Access the /Accept/ header field.

accept :: Http a :-> Maybe Parameters
accept = keyValues "," ";" `iso` id % header "Accept"

-- | Access the /Accept-Encoding/ header field.

acceptEncoding :: Http a :-> Maybe [String]
acceptEncoding = (values "," `iso` id) % header "Accept-Encoding"

-- | Access the /Accept-Language/ header field.

acceptLanguage :: Http a :-> Maybe [String]
acceptLanguage = values "," `iso` id % header "Accept-Language"

-- | Access the /Connection/ header field.

cacheControl :: Http a :-> Maybe String
cacheControl = header "Cache-Control"

-- | Access the /Keep-Alive/ header field.

keepAlive :: (Read i, Integral i) => Http a :-> Maybe i
keepAlive = (join . fmap readMay <-> fmap show) `iso` header "Keep-Alive"

-- | Access the /Cookie/ header field.

cookie :: Http Request :-> Maybe String
cookie = header "Cookie"

-- | Access the /Set-Cookie/ header field.

setCookie :: Http Response :-> Maybe String
setCookie = header "Set-Cookie"

-- | Access the /Location/ header field.

location :: Http a :-> Maybe String
location = header "Location"

-- | Access the /Content-Type/ header field. The content-type will be parsed
-- into a mimetype and optional charset.

contentType :: Http a :-> Maybe (String, Maybe String)
contentType = (parser <-> fmap printer) `iso` (keyValues ";" "=" `iso` id % header "Content-Type")
  where 
    parser a = 
      case a of
        Just ((m, Nothing):("charset", c):_) -> Just (m, c)
        _                                    -> Nothing
    printer (x, y) = (x, Nothing) : maybe [] (\z -> [("charset", Just z)]) y

-- | Access the /Data/ header field.

date :: Http a :-> Maybe String
date = header "Date"

-- | Access the /Host/ header field.

hostname :: Http a :-> Maybe String
hostname = header "Host"

-- | Access the /Server/ header field.

server :: Http a :-> Maybe String
server = header "Server"

-- | Access the /User-Agent/ header field.

userAgent :: Http a :-> Maybe String
userAgent = header "User-Agent"

