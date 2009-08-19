module Network.Salvia.Handler.Printer {- todo doc -}
  ( hRequestPrinter
  , hResponsePrinter
  , hFlushRequest
  , hFlushResponse
  , hFlushHeaders
  , hFlushQueue
  )
where

import Misc.Util
import Control.Monad.State
import Network.Salvia.Core.Aspects
import Network.Protocol.Http
import System.IO

{- |
The 'hPrinter' handler prints the entire HTTP message including the headers to
the other endpoint. This handler is generally used as (one of) the last handler
in a client or server environment.
-}

hRequestPrinter :: FlushM Request m => m ()
hRequestPrinter = hFlushRequest >> flushQueue forRequest

hResponsePrinter :: FlushM Response m => m ()
hResponsePrinter = hFlushResponse >> flushQueue forResponse

hFlushRequest :: FlushM Request m => m ()
hFlushRequest = flushHeaders forRequest

hFlushResponse :: FlushM Response m => m ()
hFlushResponse = flushHeaders forResponse

-- | Send all the message headers directly over the socket.

hFlushHeaders :: forall m d. (Show (HTTP d), SocketM m, SendM m, MonadIO m, HttpM d m) => Side d -> m ()
hFlushHeaders Side =
  do r <- http get :: m (HTTP d)
     h <- sock 
     catchIO (hPutStr h (show r) >> hFlush h) ()

hFlushQueue :: (SendM m, SocketM m, MonadIO m) => m ()
hFlushQueue =
  do s <- rawSock
     h <- sock
     sendall s h
     catchIO (hFlush h) ()
  where sendall s h =
          dequeue >>=
          maybe (return ()) (\a -> catchIO (a (s, h)) () >> sendall s h)

