{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Network.Salvia.Core.Handler where

import Control.Applicative
import Control.Monad.State
import Data.Monoid
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

instance Monoid a => Monoid (Handler a) where
  mempty = mzero >> return mempty
  mappend = mplus

instance Alternative Handler where
  empty = mzero
  (<|>) = mplus

instance MonadPlus Handler where
  mzero = Handler $
    do setM (status % response) BadRequest
       return (error "mzero/empty")
  a `mplus` b =
    do r <- a
       s <- getM (status % response)
       if statusFailure s
         then A.reset >> b
         else return r

-- This Handler allows for a concrete implementation of all server aspects.

instance A.Config Handler where
  config  = getM config

instance A.Client Handler where
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
  rawSock = getM rawSock
  sock    = getM sock
  raw     = send

{- |
Queue one potential send action in the send queue. This will not (yet) be sent
over the socket.
-}

send :: SendAction -> Handler ()
send f = modM queue (++[f])

instance A.Send Handler where
  sendStr s     = send (flip U.hPutStr s . snd)
  sendBs bs     = send (flip B.hPutStr bs . snd)
  spoolStr f fd = send (\(_, h) -> U.hGetContents fd >>= \d -> U.hPutStr h (f d))
  spoolBs  f fd = send (\(_, h) -> B.hGetContents fd >>= \d -> B.hPut h (f d))

  flushHeaders  = flushHeaders
  flushQueue    = flushQueue
  emptyQueue    = emptyQueue

{- | Reset the send queue by throwing away all potential send actions. -}

emptyQueue :: Handler ()
emptyQueue = setM queue []

{- | Send all the response headers directly over the socket. -}

flushHeaders :: Handler ()
flushHeaders =
  do r <- getM response
     h <- getM sock 
     liftIO (hPutStr h (showMessageHeader r) >> hFlush h)

{- | Apply all send actions successively to the client socket. -}

flushQueue :: Handler ()
flushQueue =
  do s <- getM rawSock
     h <- getM sock
     q <- getM queue
     liftIO (mapM_ ($ (s, h)) q >> hFlush h)

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
handlerContents =
  do len <- getM (contentLength % request)
     kpa <- getM (keepAlive     % request)
     s   <- getM sock
     liftIO $
       case (kpa :: Maybe Integer, len :: Maybe Integer) of
         (_,       Just n)  -> liftM Just (B.hGet s (fromIntegral n))
         (Nothing, Nothing) -> liftM Just (B.hGetContents s)
         _                  -> return Nothing

