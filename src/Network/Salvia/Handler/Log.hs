module Network.Salvia.Handler.Log {- doc ok -}
  ( hLog
  , hLogWithCounter
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.State
import Data.Record.Label
import Misc.Terminal (red, green, reset)
import Network.Protocol.Http
import Network.Salvia.Core.Aspects hiding (reset)
import System.IO

{- |
A simple logger that prints a summery of the request information to the
specified file handle.
-}

hLog :: (SocketM m, MonadIO m, ResponseM m, RequestM m) => Handle -> m ()
hLog = logger Nothing

{- | Like `hLog` but also prints the request count since server startup. -}

hLogWithCounter :: (SocketM m, MonadIO m, ResponseM m, RequestM m) => TVar Int -> Handle -> m ()
hLogWithCounter a = logger (Just a)

logger :: (SocketM m, MonadIO m, ResponseM m, RequestM m) => Maybe (TVar Int) -> Handle -> m ()
logger count handle =
  do c <- case count of
       Nothing -> return ""
       Just c' -> liftIO (show <$> atomically (readTVar c'))
     mt   <- request  (getM method)
     ur   <- request  (getM uri)
     st   <- response (getM status)
     addr <- peer
     let code = codeFromStatus st
         clr  = if code >= 400 then red else green
     liftIO $ hPutStrLn handle $ concat [
         concat ["[", show addr, "] ", c, "\t"]
       , show mt, "\t"
       , show ur, " -> "
       , clr
       , show code, " "
       , show st
       , reset
       ]

