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
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Monoid
import Data.Record.Label hiding (get)
import Network.Protocol.Http hiding (hostname)
import Network.Salvia.Handler.Body
import Network.Salvia.Handler.Close
import Network.Salvia.Handler.Login
import Network.Salvia.Handler.Printer
import Network.Salvia.Handler.Session
import Network.Salvia.Impl.Context
import Network.Salvia.Interface
import Prelude hiding (mod)
import Safe
import System.IO
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Record.Label as L

newtype Handler p a = Handler { unHandler :: StateT (Context p) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (Context p))

runHandler :: Handler p a -> Context p -> IO (a, Context p)
runHandler h = runStateT (unHandler h)

instance ForkM IO (Handler p) where
  forkM a = get >>= return . fmap fst . runHandler a

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

instance QueueM (Handler p) where
  enqueue f = modM cQueue (++[f])
  dequeue   = headMay <$> getM cQueue <* modM cQueue (tailDef [])

instance SendM (Handler p) where
  send        s    = enqueue (\(_, h) -> hSetEncoding h utf8 >> ByteString.hPut h (fromString s))
  sendBs      bs   = enqueue (\(_, h) -> ByteString.hPutStr h bs)
  spoolWith   f fd = enqueue (\(_, h) -> hGetContents fd >>= ByteString.hPut h . fromString . f)
  spoolWithBs f fd = enqueue (\(_, h) -> ByteString.hGetContents fd >>= ByteString.hPut h . f)

instance SockM (Handler p) where
  socket = getM cSocket
  handle = getM cHandle

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
  host   = getM cServerHost
  admin  = getM cAdminMail
  listen = getM cListenOn

instance Contains p (TVar q) => PayloadM p q (Handler p) where
  payload st =
    do pl <- getM cPayload :: Handler p p
       let var = L.get select pl :: TVar q
       liftIO . atomically $
          do q <- readTVar var
             let (s, q') = runState st q
             writeTVar var q'
             return s

instance Contains q (TVar (Sessions p)) => SessionM p (Handler q) where
  prolongSession = hProlongSession (undefined :: p)
  getSession     = hGetSession
  putSession     = hPutSession
  delSession     = hDelSession     (undefined :: p)
  withSession    = hWithSession

instance ( Contains q (TVar (Sessions (UserPayload p)))
         , Contains q (TVar UserDatabase)
         ) => LoginM p (Handler q) where
  login      = hLogin      (undefined :: p)
  logout     = hLogout     (undefined :: p)
  loginfo    = hLoginfo    (undefined :: p)
  signup     = hSignup     (undefined :: p)
  authorized = hAuthorized (undefined :: p)

