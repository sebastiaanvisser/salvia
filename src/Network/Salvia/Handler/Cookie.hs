{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.Cookie where

import Control.Applicative hiding (empty)
import Control.Category
import Data.Record.Label
import Data.Time.Format
import Network.Protocol.Cookie
import Network.Salvia.Interface
import Network.Socket
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
root. If the second argument is set, the cookie will be valid for all
subdomains.
-}

hNewCookie :: (ServerM m, ServerAddressM m, FormatTime t) => t -> Bool -> m Cookie
hNewCookie expire wildcard = do
  hst   <- host
  sAddr <- serverAddress
  return 
    . (path    `set` Just "/")
    . (domain  `set` Just ((if wildcard then ('.':) else id) hst))
    . (port    `set` [portNum sAddr])
    . (expires `set` Just (formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" expire))
    $ empty
  where portNum (SockAddrInet  p _)     = fromIntegral p
        portNum (SockAddrInet6 p _ _ _) = fromIntegral p
        portNum _                       = -1

