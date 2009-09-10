module Network.Salvia.Handler.Log {- doc ok -}
  ( hLog
  , hLogWithCounter
  )
where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.State
import Data.List
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Core.Aspects hiding (server)
import System.IO

{- |
A simple logger that prints a summery of the request information to the
specified file handle.
-}

hLog :: (PeerM m, MonadIO m, HttpM Response m, HttpM Request m) => Handle -> m ()
hLog = logger Nothing

{- | Like `hLog` but also prints the request count since server startup. -}

hLogWithCounter :: (PeerM m, MonadIO m, HttpM Response m, HttpM Request m) => TVar Int -> Handle -> m ()
hLogWithCounter a = logger (Just a)

logger :: (PeerM m, MonadIO m, HttpM Response m, HttpM Request m) => Maybe (TVar Int) -> Handle -> m ()
logger count handle =
  do c <- case count of
       Nothing -> return "-"
       Just c' -> liftIO (show <$> atomically (readTVar c'))
     mt   <- request  (getM method)
     ur   <- request  (getM uri)
     st   <- response (getM status)
     dt   <- response (getM date)
     addr <- peer
     let code = codeFromStatus st
     liftIO
       $ hPutStrLn handle
       $ intercalate " ; "
         [ maybe "" id dt
         , c
         , show addr
         , show mt
         , ur
         , show code ++ " " ++ show st
         ]

