module Network.Salvia.Handler.Cookie
( hSetCookie
, hCookie
, hDelCookie
, hNewCookie
)
where

import Control.Applicative hiding (empty)
import Control.Category
import Data.Record.Label
import Data.Time.Format
import Network.Protocol.Cookie
import Network.Salvia.Core.Aspects
import Network.Salvia.Core.Config
import Prelude hiding ((.), id)
import System.Locale
import qualified Network.Protocol.Http as H

{- | Set the `Set-Cookie` HTTP response header with the specified `Cookies`. -}

hSetCookie :: HttpM H.Response m => Cookies -> m ()
hSetCookie = response . setM H.setCookie . Just . show

{- | Try to get the cookies from the HTTP `cookie` request header. -}

hCookie :: (HttpM H.Request m) => m (Maybe Cookies)
hCookie = fmap (fw cookies) <$> request (getM H.cookie)

{- | Delete one cookie by removing it from the `Set-Cookie' header. -}

hDelCookie :: HttpM H.Response m => String -> m ()
hDelCookie nm = response (theCookie =: Just Nothing)
  where theCookie = fmapL (pickCookie nm)
                  . fmapL (cookies `iso` id)
                  . H.setCookie

{- |
Convenient method for creating cookies that expire in the near future and are
bound to the domain and port this server runs on. The path will be locked to
root.
-}

hNewCookie :: (ServerM m, FormatTime t) => t -> m Cookie
hNewCookie expire = do
  httpd <- server
  return 
    . (path    `set` Just "/")
    . (domain  `set` Just ('.' : hostname httpd))
    . (port    `set` [fromEnum (listenPort httpd)])
    . (expires `set` Just (formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" expire))
    $ empty

