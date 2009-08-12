module Network.Salvia.Handler.Banner (hBanner) where {- doc ok -}

import Control.Applicative
import Control.Monad.State
import Data.Record.Label
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Network.Protocol.Http
import Network.Salvia.Core.Aspects
import System.Locale

{- |
The 'hBanner' handler adds the current date-/timestamp and a custom server name
to the response headers.
-}

hBanner
  :: (MonadIO m, ResponseM m)
  => String -- ^ The HTTP server name.
  -> m ()
hBanner sv = do
  dt <- liftIO $
    do zone <- getCurrentTimeZone
       time <- utcToLocalTime zone <$> getCurrentTime
       return $ formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" time
  response $
    do setM date   (Just dt)
       setM server (Just sv)

