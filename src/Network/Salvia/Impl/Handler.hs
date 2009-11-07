{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances #-}
module Network.Salvia.Impl.Handler where

import Control.Applicative
import Control.Monad.State
import Data.Monoid
import Data.Record.Label hiding (get)
import Network.Protocol.Http
import Network.Salvia.Core.Config
import Network.Salvia.Core.Context
import Network.Salvia.Handler.Body
import Network.Salvia.Handler.Printer
import Network.Salvia.Handler.Session
import Network.Salvia.Handler.Login
import Safe
import System.IO
import Network.Salvia.Core.Aspects
import qualified Data.ByteString as ByteString

newtype Handler c p a = Handler { unHandler :: StateT (Context c p) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (Context c p))

runHandler :: Handler c p a -> Context c p -> IO (a, Context c p)
runHandler h = runStateT (unHandler h)

type ServerHandler p a = Handler Config p a
type ClientHandler   a = Handler () () a

instance HttpM Request (Handler c p) where
  http st =
    do (a, s) <- runState st <$> getM cRequest
       cRequest =: s >> return a

instance HttpM Response (Handler c p) where
  http st =
    do (a, s) <- runState st <$> getM cResponse
       cResponse =: s >> return a

instance QueueM (Handler c p) where
  enqueue f     = modM cQueue (++[f])
  dequeue       = headMay <$> getM cQueue <* modM cQueue (tailDef [])

instance SendM (Handler c p) where
  send        s    = enqueue (flip hPutStr s . snd)
  sendBs      bs   = enqueue (flip ByteString.hPutStr bs . snd)
  spoolWith   f fd = enqueue (\(_, h) -> hGetContents fd >>= hPutStr h . f)
  spoolWithBs f fd = enqueue (\(_, h) -> ByteString.hGetContents fd >>= ByteString.hPut h . f)

instance SockM (Handler c p) where
  rawSock = getM cRawSock
  sock    = getM cSock

instance ClientAddressM (Handler c p) where
  clientAddress = getM cClientAddr

instance ServerAddressM (Handler c p) where
  serverAddress = getM cServerAddr

instance Monoid a => Monoid (Handler c p a) where
  mempty  = mzero >> return mempty
  mappend = mplus

instance Alternative (Handler c p) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus (Handler c p) where
  mzero =
    do http (status =: BadRequest)
       return (error "mzero/empty")
  a `mplus` b =
    do r <- a
       s <- http (getM status)
       if statusFailure s
         then http (put emptyResponse) >> mzero >> b
         else return r

instance FlushM Response (Handler c p) where
  flushHeaders = hFlushHeaders
  flushQueue _ = hFlushQueue

instance FlushM Request (Handler c p) where
  flushHeaders = hFlushHeaders
  flushQueue _ = hFlushQueue

instance BodyM Request (Handler c p) where
  body = hRawBody

instance BodyM Response (Handler c p) where
  body = hRawBody

instance PayloadM (Handler c p) p where
  payload st =
    do (a, s) <- runState st <$> getM cPayload
       cPayload =: s >> return a

instance ServerM (Handler Config p) where
  server = getM cConfig

instance ClientM (Handler () ()) where
  client = return ()

instance SessionM (Handler Config (Sessions p)) p where
  prolongSession = hProlongSession
  getSession     = hGetSession
  putSession     = hPutSession
  delSession     = hDelSession
  withSession    = hWithSession

instance LoginM (Handler Config (Sessions (UserPayload p))) p where
  signup     = hSignup
  login      = hLogin
  logout     = hLogout
  authorized = hAuthorized

