module Network.Salvia.Handler.Close
  ( hCloseConn
  , hKeepAlive
  , emptyQueue
  )
where

import Control.Monad.State
import Data.Maybe
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Interface
import Network.Salvia.Handler.Error
import System.IO

-- | Run a handler once and close the connection afterwards.

hCloseConn :: (HandleM m, MonadIO m) => m a -> m ()
hCloseConn h = h >>
  do handleIn  >>= flip catchIO () . hClose
     handleOut >>= flip catchIO () . hClose

{- |
Run a handler and keep the connection open for potential consecutive requests.
The connection will only be closed after a request finished and one or more of
the following criteria are met:

* There is no `contentLength` set in the response headers. When this is the
  case the connection cannot be kept alive.

* The client has set the `connection` header field to 'close'.

* The connection has already been closed, possible due to IO errors.

* The HTTP version is HTTP/1.0.
-}

hKeepAlive :: (QueueM m, HandleM m, HttpM' m, MonadIO m) => m a -> m ()
hKeepAlive handler =
  do _ <- handler
     hin    <- handleIn
     hout   <- handleOut
     conn   <- request (getM connection)
     ver    <- request (getM version)
     len    <- response (getM contentLength)
     closed <- liftIO $ liftM2 (||) (hIsClosed hin) (hIsClosed hout)
     if or [ closed
           , conn == Just "Close"
           , isNothing (len :: Maybe Integer)
           , ver == http10
           ]
       then catchIO (hClose hin >> hClose hout) ()
       else resetContext >> hKeepAlive handler 

resetContext :: (HttpM' m, QueueM m) => m ()
resetContext =
  do request  (put emptyRequest)
     response (put emptyResponse)
     emptyQueue

-- | Empty the send queue.

emptyQueue :: QueueM m => m ()
emptyQueue = dequeue >>= return () `maybe` const emptyQueue

