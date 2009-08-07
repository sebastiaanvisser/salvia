module Network.Salvia.Handler.Banner (hBanner) where

import Control.Monad.State
import Data.Record.Label
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import Network.Protocol.Http
import Network.Salvia.Httpd
import System.Locale (defaultTimeLocale)

{- |
The 'hBanner' handler adds the current date-/timestamp and a custom server name
to the response headers.
-}

hBanner
  :: (MonadIO m, Response m)
  => String     -- ^ The HTTP server name.
  -> m ()
hBanner sv = do
  dt <- liftIO $ do
    zone <- getCurrentTimeZone
    time <- liftM (utcToLocalTime zone) getCurrentTime
    return $ formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" time
  response $
    do setM date   dt
       setM server sv

