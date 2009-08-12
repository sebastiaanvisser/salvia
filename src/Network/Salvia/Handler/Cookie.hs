module Network.Salvia.Handler.Cookie {- doc ok -}
  ( hSetCookies
  , hGetCookies

  , newCookie
  ) where

import Control.Applicative hiding (empty)
import Control.Monad.State
import Data.Record.Label
import Data.Time.Format
import Network.Protocol.Cookie hiding (cookie)
import Network.Protocol.Http
import Network.Salvia.Core.Config
import Network.Salvia.Core.Aspects
import System.Locale

{- | Set the `cookie` HTTP response header (Set-Cookie) with the specified `Cookies`. -}

hSetCookies :: ResponseM m => Cookies -> m ()
hSetCookies = response . (setCookie =:) . Just . showCookies

{- | Try to get the cookies from the HTTP `cookie` request header. -}

hGetCookies :: RequestM m => m (Maybe Cookies)
hGetCookies = join . fmap parseCookies <$> request (getM cookie)

{- |
Convenient method for creating cookies that expire in the near future and are
bound to the domain and port this server runs on. The path will be locked to
root.
-}

newCookie :: (ConfigM m, FormatTime t) => t -> m Cookie
newCookie expire = do
  httpd <- config
  return $ empty {
      path    = Just "/"
--  , domain  = Just $ '.' : hostname httpd
    , port    = [fromEnum $ listenPort httpd]
    , expires = Just $ formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" expire
    }

