module Network.Salvia.Handler.Printer {- doc ok -}
  ( hRequestPrinter
  , hResponsePrinter
  , hFlushRequest
  , hFlushResponse
  ) where

import Control.Monad.State
import Network.Salvia.Core.Aspects
import System.IO

{- |
The 'hRequestPrinter' handler prints the entire HTTP request message including
the headers to the server.
-}

hRequestPrinter :: SendM m => m ()
hRequestPrinter = flushRequest >> flushQueue

{- |
The 'hResponsePrinter' handler prints the entire HTTP response message including
the headers to the client. This handler is generally used as (one of) the last
handler in a server environment.
-}

hResponsePrinter :: SendM m => m ()
hResponsePrinter = flushResponse >> flushQueue

{- | Send all the request headers directly over the socket. -}

hFlushRequest :: (RequestM m, MonadIO m, SocketM m) => m ()
hFlushRequest =
  do r <- request get
     h <- sock 
     liftIO (hPutStr h (show r) >> hFlush h)

{- | Send all the response headers directly over the socket. -}

hFlushResponse :: (ResponseM m, MonadIO m, SocketM m) => m ()
hFlushResponse =
  do r <- response get
     h <- sock 
     liftIO (hPutStr h (show r) >> hFlush h)

