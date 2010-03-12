{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, RankNTypes #-}
module Network.Salvia.Handler.Printer
( hRequestPrinter
, hResponsePrinter
, hFlushHeaders
, hFlushRequestHeaders
, hFlushResponseHeaders
, hFlushQueue
)
where

import Control.Applicative
import Control.Monad.State
import Network.Protocol.Http
import Network.Salvia.Interface
import Network.Salvia.Handler.Error
import System.IO

{- |
The 'hRequestPrinter' handler prints the entire HTTP request including the
headers and the body to the socket towards the server. This handler is
generally used as (one of) the last handler in a server environment.
-}

hRequestPrinter :: FlushM Request m => m ()
hRequestPrinter = flushHeaders forRequest >> flushQueue forRequest

{- |
The 'hResponsePrinter' handler prints the entire HTTP response including the
headers and the body to the socket towards the client. This handler is
generally used as (one of) the last handler in a client environment.
-}

hResponsePrinter :: FlushM Response m => m ()
hResponsePrinter = flushHeaders forResponse >> flushQueue forResponse

-- | Send all the message headers directly over the socket.

-- | todo: printer for rawResponse over response!!

hFlushHeaders :: forall m d. (Show (Http d), HandleM m, QueueM m, MonadIO m, HttpM d m) => d -> m ()
hFlushHeaders _ =
  do r <- http get :: m (Http d)
     h <- handle 
     catchIO (hPutStr h (show r) >> hFlush h) ()

-- | Like `hFlushHeaders` but specifically for the request headers.

hFlushRequestHeaders :: FlushM Request m => m ()
hFlushRequestHeaders = flushHeaders forRequest

-- | Like `hFlushHeaders` but specifically for the response headers.

hFlushResponseHeaders :: FlushM Response m => m ()
hFlushResponseHeaders = flushHeaders forResponse

-- | One by one apply all enqueued send actions to the socket.

hFlushQueue :: (QueueM m, HandleM m, SocketM m, MonadIO m) => m ()
hFlushQueue =
  do s <- socket
     h <- handle
     q <- queue
     flip catchIO () $
       sequence_ (map (\(SendAction f) -> f (s, h)) q) >> hFlush h
  where queue = dequeue >>= maybe (return []) ((<$> queue) . (:))


