module Network.Salvia.Handler.Close (
    hCloseConn
  , hKeepAlive
  ) where

import Control.Monad.State
import Data.Maybe
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Httpd
import System.IO

{- |
Run a handler once and close the connection afterwards.
-}

hCloseConn :: (Socket m, MonadIO m) => m a -> m ()
hCloseConn h = h >> sock >>= liftIO . hClose

{- |
Run a handler and keep the connection open for potential consecutive requests.
The connection will only be closed after a request finished and one or more of
the following criteria are met:

* There is no `contentLength` set in the response headers. When this is the
  case the connection cannot be kept alive.

* The client has set the `connection` header field to 'close'.

* The connection has already been closed, possible due to IO errors.
-}

hKeepAlive :: (MonadIO m, Socket m, Request m, Response m) => m a -> m ()
hKeepAlive handler =
  do handler
     h      <- sock
     conn   <- request (getM connection)
     ver    <- request (getM version)
     len    <- response (getM contentLength)
     closed <- liftIO (hIsClosed h)
     if or [ closed
           , conn == "Close"
           , isNothing (len::Maybe Integer)
           , ver == http10
           ]
       then liftIO (hClose h)
       else resetContext >> hKeepAlive handler

resetContext :: (Socket m, Request m, Response m) => m ()
resetContext =
  do request  (put emptyRequest)
     response (put emptyResponse)
     emptyQueue

