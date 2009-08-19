module Network.Salvia.Core.Handler where

import Control.Applicative
import Control.Monad.State
import Data.Monoid
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Core.Config
import Network.Salvia.Core.Context
import Network.Salvia.Handler.Body
import Network.Salvia.Handler.Printer
import Safe
import System.IO
import qualified Data.ByteString.Lazy as B
import qualified Network.Salvia.Core.Aspects as A
import qualified System.IO.UTF8 as U

-- | CURRENTLY ONLY SERVER IMPLEMENTATION

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
    do status % response =: BadRequest
       return (error "mzero/empty")
  a `mplus` b =
    do r <- a
       s <- getM (status % response)
       if statusFailure s
         then A.http (put emptyResponse) >> empty >> b
         else return r

instance A.ConfigM (Handler Config p) where
  config  = getM config

instance A.HttpM Request (Handler c p) where
  http st =
    do (a, s') <- runState st <$> getM request
       request =: s'
       return a

instance A.HttpM Response (Handler c p) where
  http st =
    do (a, s') <- runState st <$> getM response
       response =: s'
       return a

instance A.SocketM (Handler c p) where
  rawSock = getM rawSock
  sock    = getM sock
  peer    = getM peer

-- toto: Encoding!!!!
instance A.SendM (Handler c p) where
  enqueue f     = modM queue (++[f])
  dequeue       = headMay <$> getM queue <* modM queue (tailDef [])

  sendStr s     = A.enqueue (flip U.hPutStr s . snd)
  sendBs bs     = A.enqueue (flip B.hPutStr bs . snd)
  spoolStr f fd = A.enqueue (\(_, h) -> U.hGetContents fd >>= U.hPutStr h . f)
  spoolBs  f fd = A.enqueue (\(_, h) -> B.hGetContents fd >>= B.hPut    h . f)

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

