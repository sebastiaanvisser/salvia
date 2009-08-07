{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Network.Salvia.Core.Handler where

import Control.Applicative
import Control.Monad.State
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Core.Context
import System.IO
import qualified Data.ByteString.Lazy as B
import qualified Network.Salvia.Core.Aspects as A
import qualified System.IO.UTF8 as U

{- |
A HTTP request handler lives in `IO` and carries a server context in the
`State` monad. This module also provides an `Applicative` instance for `StateT`
`Context` `IO`.
-}

newtype Handler a = Handler { unHandler :: StateT Context IO a }
  deriving (Functor, Applicative, Monad, MonadState Context, MonadIO)

instance A.Config Handler where
  config  = getM config
  address = getM address

instance A.Request Handler where
  request st =
    do (a, s') <- runState st `liftM` getM request
       setM request s'
       return a

instance A.Response Handler where
  response st =
    do (a, s') <- runState st `liftM` getM response
       setM response s'
       return a

instance A.Socket Handler where
  sock         = getM sock
  flushHeaders = flushHeaders
  flushQueue   = flushQueue
  emptyQueue   = emptyQueue
  reset        = reset

{- | Reset the send queue by throwing away all potential send actions. -}

emptyQueue :: Handler ()
emptyQueue = setM queue []

{- | Reset both the send queue and the generated server response. -}

reset :: Handler ()
reset = do
  setM response emptyResponse
  emptyQueue

{- | Send all the response headers directly over the socket. -}

flushHeaders :: Handler ()
flushHeaders = do
  r <- getM response
  s <- getM sock 
  liftIO $ hPutStr s (showMessageHeader r)

{- | Apply all send actions successively to the client socket. -}

flushQueue :: Handler ()
flushQueue =
  do h <- getM sock
     q <- getM queue
     liftIO $
       do mapM_ ($ h) q
          hFlush h

{- |
Queue one potential send action in the send queue. This will not (yet) be sent
over the socket.
-}

send :: SendAction -> Handler ()
send f = modM queue (++[f])

instance A.Send Handler where
  sendStr s     = send (flip U.hPutStr s)
  sendBs bs     = send (flip B.hPutStr bs)
  spoolStr f fd = send (\s -> U.hGetContents fd >>= \d -> U.hPutStr s (f d))
  spoolBs  f fd = send (\s -> B.hGetContents fd >>= \d -> B.hPut s (f d))

instance A.Receive Handler where
  contents = handlerContents

{- |
First (possibly naive) handler to retreive the client request body as a
`B.ByteString`. This probably does not handle all the quirks that the HTTP
protocol specifies, but it does the job for now. When a 'contentLength' header
field is available only this fixed number of bytes will read from the socket.
When neither the 'keepAlive' and 'contentLength' header fields are available
the entire payload of the request will be read from the socket. This method is
probably only useful in the case of 'PUT' request, because no decoding of
'POST' data is handled.
-}

handlerContents :: Handler (Maybe B.ByteString)
handlerContents = do
  len <- getM (contentLength % request)
  kpa <- getM (keepAlive     % request)
  s   <- getM sock
  liftIO $
    case (kpa :: Maybe Integer, len :: Maybe Integer) of
      (_,       Just n)  -> liftM Just (B.hGet s (fromIntegral n))
      (Nothing, Nothing) -> liftM Just (B.hGetContents s)
      _                  -> return Nothing

