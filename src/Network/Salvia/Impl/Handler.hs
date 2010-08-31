{-# LANGUAGE
    FlexibleInstances
  , StandaloneDeriving
  , TypeSynonymInstances
  , UndecidableInstances
  , OverlappingInstances
  , IncoherentInstances
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  #-}
module Network.Salvia.Impl.Handler where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.State
import Control.Concurrent
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.Monoid
import Data.Record.Label
import Network.Protocol.Http hiding (hostname)
import Network.Salvia.Handler.Body
import Network.Salvia.Handler.Close
import Network.Salvia.Handler.Printer
import Network.Salvia.Impl.Context
import Network.Salvia.Interface
import Prelude hiding (mod)
import Safe
import qualified Data.ByteString.Lazy as ByteString

newtype Handler p a = H { unH :: StateT (Context p) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (Context p))

runHandler :: Handler p a -> Context p -> IO (a, Context p)
runHandler h = runStateT (unH h)

instance ForkableMonad (Handler p) where
  fork a = do ctx <- H get
              liftIO (forkIO (runHandler a ctx >> return ()))

instance HttpM Request (Handler p) where
  http st =
    do (a, s) <- runState st <$> getM cRequest
       cRequest =: s >> return a

instance HttpM Response (Handler p) where
  http st =
    do (a, s) <- runState st <$> getM cResponse
       cResponse =: s >> return a

instance RawHttpM Request (Handler p) where
  rawHttp st =
    do (a, s) <- runState st <$> getM cRawRequest
       cRawRequest =: s >> return a

instance RawHttpM Response (Handler p) where
  rawHttp st =
    do (a, s) <- runState st <$> getM cRawResponse
       cRawResponse =: s >> return a

instance HandleQueueM (Handler p) where
  enqueueHandle f = modM cQueue (++[SendAction (f . snd)])

instance SocketQueueM (Handler p) where
  enqueueSock f = modM cQueue (++[SendAction (f . fst)])

instance QueueM (Handler p) where
  dequeue = headMay <$> getM cQueue <* modM cQueue (tailDef [])

instance SendM (Handler p) where
  send        s    = enqueueHandle (\h -> ByteString.hPut h (fromString s))
  sendBs      bs   = enqueueHandle (\h -> ByteString.hPutStr h bs)
  spoolWith   f fd = enqueueHandle (\h -> ByteString.hGetContents fd >>= ByteString.hPut h . fromString . f . toString)
  spoolWithBs f fd = enqueueHandle (\h -> ByteString.hGetContents fd >>= ByteString.hPut h . f)

instance SocketM (Handler p) where
  socket = getM cSocket

instance HandleM (Handler p) where
  handleIn  = getM cHandleIn
  handleOut = getM cHandleOut

instance ClientAddressM (Handler p) where
  clientAddress = getM cClientAddr

instance ServerAddressM (Handler p) where
  serverAddress = getM cServerAddr

instance Monoid a => Monoid (Handler p a) where
  mempty  = mzero >> return mempty
  mappend = mplus

instance Alternative (Handler p) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus (Handler p) where
  mzero =
    do http (status =: BadRequest)
       return (error "mzero/empty")
  a `mplus` b =
    do r <- a
       s <- http (getM status)
       if statusFailure s
         then do response $
                   do status        =: OK
                      contentLength =: (Nothing :: Maybe Integer)
                 emptyQueue >> mzero >> b
         else return r

instance FlushM Response (Handler p) where
  flushHeaders = hFlushHeaders
  flushQueue _ = hFlushQueue

instance FlushM Request (Handler p) where
  flushHeaders = hFlushHeaders
  flushQueue _ = hFlushQueue

instance BodyM Request (Handler p) where
  body = hRawBody

instance BodyM Response (Handler p) where
  body = hRawBody

instance ServerM (Handler p) where
  serverHost  = getM cServerHost
  serverAdmin = getM cAdminMail
  listen      = getM cListenOn

instance Contains p (TVar q) => PayloadM p q (Handler p) where
  payload st =
    do pl <- getM cPayload :: Handler p p
       let var = getL select pl :: TVar q
       liftIO . atomically $
          do q <- readTVar var
             let (s, q') = runState st q
             writeTVar var q'
             return s

