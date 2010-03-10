{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.Banner (hBanner) where

import Control.Applicative
import Control.Monad.State
import Data.Record.Label
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Network.Protocol.Http
import Network.Salvia.Interface
import System.Locale

{- |
The 'hBanner' handler adds the current date-/timestamp and a custom server name
to the response headers.
-}

hBanner
  :: (MonadIO m, HttpM Response m)
  => String -- ^ The name to include as the /Server/ header line.
  -> m ()
hBanner sv =
  do dt <- liftIO $
       do zone <- getCurrentTimeZone
          time <- utcToLocalTime zone <$> getCurrentTime
          return $ formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z" time
     response $
       do date   =: Just dt
          server =: Just sv

