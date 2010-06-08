{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.Log where

import Control.Applicative
import Control.Monad.State
import Data.List
import Data.Record.Label
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Network.Protocol.Http
import Network.Salvia.Interface
import System.IO
import System.Locale

{- |
A simple logger that prints a summery of the request information to
the specified file handle.
-}

hLog :: (AddressM' m , MonadIO m, HttpM' m) => Handle -> m ()
hLog h =
  do mt <- request  (getM method)
     ur <- request  (getM uri)
     st <- response (getM status)
     ca <- clientAddress
     sa <- serverAddress
     dt <- liftIO $
       do zone <- getCurrentTimeZone
          time <- utcToLocalTime zone <$> getCurrentTime
          return $ formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z" time
     let code = codeFromStatus st
     liftIO
       . hPutStrLn h
       $ intercalate " ; "
         [ dt
         , show sa
         , show mt
         , show ca
         , ur
         , show code ++ " " ++ show st
         ]

-- | Dump the request headers to the standard output, useful for debugging.

hDumpRequest :: (HttpM Request m, MonadIO m) => m ()
hDumpRequest = request get >>= liftIO . print

-- | Dump the response headers to the standard output, useful for debugging.

hDumpResponse :: (HttpM Response m, MonadIO m) => m ()
hDumpResponse = response get >>= liftIO . print

