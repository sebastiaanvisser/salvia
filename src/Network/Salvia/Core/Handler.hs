{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Network.Salvia.Core.Handler where

import Control.Applicative
import Control.Monad.State
import Data.Monoid
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Core.Config
import Network.Salvia.Core.Context
import Network.Salvia.Handler.Contents
import Network.Salvia.Handler.Printer
import System.IO
import qualified Data.ByteString.Lazy as B
import qualified Network.Salvia.Core.Aspects as A
import qualified System.IO.UTF8 as U

{- |
A HTTP request handler lives in `IO` and carries a server context in the
`State` monad. This module also provides an `Applicative` instance for `StateT`
`Context` `IO`.
-}

newtype Handler c p a = Handler { unHandler :: StateT (Context c p) IO a }
  deriving (Functor, Applicative, Monad, MonadState (Context c p), MonadIO)

instance Monoid a => Monoid (Handler c p a) where
  mempty = mzero >> return mempty
  mappend = mplus

instance Alternative (Handler c p) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus (Handler c p) where
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

instance A.ConfigM (Handler Config p) where
  config  = getM config

instance A.RequestM (Handler c p) where
  request st =
    do (a, s') <- runState st <$> getM request
       setM request s'
       return a

instance A.ResponseM (Handler c p) where
  response st =
    do (a, s') <- runState st <$> getM response
       setM response s'
       return a

instance A.SocketM (Handler c p) where
  rawSock = getM rawSock
  sock    = getM sock
  raw f   = modM queue (++[f])
  peer    = getM peer

{- |
Queue one potential send action in the send queue. This will not (yet) be sent
over the socket.
-}

instance A.SendM (Handler c p) where
  sendStr s     = A.raw (flip U.hPutStr s . snd)
  sendBs bs     = A.raw (flip B.hPutStr bs . snd)
  spoolStr f fd = A.raw (\(_, h) -> U.hGetContents fd >>= \d -> U.hPutStr h (f d))
  spoolBs  f fd = A.raw (\(_, h) -> B.hGetContents fd >>= \d -> B.hPut h (f d))

  flushRequest  = hFlushRequest
  flushResponse = hFlushResponse
  flushQueue    = flushQueue
  emptyQueue    = setM queue []

{- | Apply all send actions successively to the client socket. -}

flushQueue :: Handler c p ()
flushQueue =
  do s <- getM rawSock
     h <- getM sock
     q <- getM queue
     liftIO (mapM_ ($ (s, h)) q >> hFlush h)

instance A.ContentsM (Handler c p) where
  contents = hRequestContents

