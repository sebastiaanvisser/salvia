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
import Safe
import qualified Network.Salvia.Core.Aspects as A

newtype Handler c p a = Handler { unHandler :: StateT (Context c p) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (Context c p))

runHandler :: Handler c p a -> Context c p -> IO (a, Context c p)
runHandler h = runStateT (unHandler h)

type ServerHandler p a = Handler Config p a
type ClientHandler   a = Handler () () a

instance A.HttpM Request (Handler c p) where
  http st =
    do (a, s) <- runState st <$> getM request
       request =: s >> return a

instance A.HttpM Response (Handler c p) where
  http st =
    do (a, s) <- runState st <$> getM response
       response =: s >> return a

instance A.QueueM (Handler c p) where
  enqueue f     = modM queue (++[f])
  dequeue       = headMay <$> getM queue <* modM queue (tailDef [])

instance A.PeerM (Handler c p) where
  rawSock = getM rawSock
  sock    = getM sock
  peer    = getM peer

instance Monoid a => Monoid (Handler c p a) where
  mempty  = mzero >> return mempty
  mappend = mplus

instance Alternative (Handler c p) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus (Handler c p) where
  mzero =
    do A.http (status =: BadRequest)
       return (error "mzero/empty")
  a `mplus` b =
    do r <- a
       s <- A.http (getM status)
       if statusFailure s
         then A.http (put emptyResponse) >> mzero >> b
         else return r

instance A.FlushM Response (Handler c p) where
  flushHeaders = hFlushHeaders
  flushQueue _ = hFlushQueue

instance A.FlushM Request (Handler c p) where
  flushHeaders = hFlushHeaders
  flushQueue _ = hFlushQueue

instance A.BodyM Request (Handler c p) where
  body = hRawBody

instance A.BodyM Response (Handler c p) where
  body = hRawBody

instance A.PayloadM (Handler c p) p where
  payload st =
    do (a, s) <- runState st <$> getM payload
       payload =: s >> return a

instance A.ServerM (Handler Config p) where
  server = getM config

instance A.ClientM (Handler () ()) where
  client = return ()

instance SessionM (Handler Config (Sessions p)) p where
  useSession  = hUseSession
  putSession  = hPutSession
  delSession  = hDelSession

