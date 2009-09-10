module Network.Salvia.Handler.Cookie {- doc ok -}
  ( hSetCookies
  , hGetCookies

  , newCookie
  )
where

import Control.Applicative hiding (empty)
import Control.Monad.State
import Data.Record.Label
import Data.Time.Format
import Network.Protocol.Cookie
import Network.Salvia.Core.Aspects
import Network.Salvia.Core.Config
import System.Locale
import qualified Network.Protocol.Http as H

{- | Set the `cookie` HTTP response header (Set-Cookie) with the specified `Cookies`. -}

hSetCookies :: HttpM H.Response m => Cookies -> m ()
hSetCookies = response . (H.setCookie =:) . Just . show

{- | Try to get the cookies from the HTTP `cookie` request header. -}

hGetCookies :: (HttpM H.Request f) => f (Maybe Cookies)
hGetCookies = fmap (fw cookies) <$> request (getM H.cookie)

{- |
Convenient method for creating cookies that expire in the near future and are
bound to the domain and port this server runs on. The path will be locked to
root.
-}

newCookie :: (ServerM m, FormatTime t) => t -> m Cookie
newCookie expire = do
  httpd <- server
  return 
    . (path    `set` Just "/")
    . (domain  `set` Just ('.' : hostname httpd))
    . (port    `set` [fromEnum (listenPort httpd)])
    . (expires `set` Just (formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" expire))
    $ empty

