module Network.Salvia.Core.Aspects where

import Control.Applicative
import Control.Monad.State
import Network.Protocol.Http
import Network.Salvia.Core.Config
import Network.Socket
import System.IO
import qualified Data.ByteString.Lazy as B

data Side s = Side

forRequest :: Side Request
forRequest = Side

forResponse :: Side Response
forResponse = Side

class (Applicative m, Monad m) => ConfigM m where
  config :: m Config

class (Applicative m, Monad m) => HttpM d m where
  http :: State (HTTP d) a -> m a

request :: HttpM Request m => State (HTTP Request) a -> m a
request = http

response :: HttpM Response m => State (HTTP Response) a -> m a
response = http

class (Applicative m, Monad m) => SocketM m where
  rawSock :: m Socket
  sock    :: m Handle
  peer    :: m SockAddr

-- TODO:  queue and dequeue are probably enough.
type SendAction = (Socket, Handle) -> IO ()

class (Applicative m, Monad m) => SendM m where
  enqueue  :: SendAction -> m ()
  dequeue  :: m (Maybe SendAction)

  sendStr  :: String                                   -> m ()
  sendBs   :: B.ByteString                             -> m ()
  spoolStr :: (String       -> String)       -> Handle -> m ()
  spoolBs  :: (B.ByteString -> B.ByteString) -> Handle -> m ()

class (Applicative m, Monad m) => FlushM d m where
  flushHeaders  :: Side d -> m ()
  flushQueue    :: Side d -> m ()

class (Applicative m, Monad m) => BodyM d m where
  body :: Side d -> m (Maybe B.ByteString)

