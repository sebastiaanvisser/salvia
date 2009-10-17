{-# LANGUAGE UndecidableInstances #-}
module Network.Salvia.Core.Aspects where

import Control.Applicative
import Control.Monad.State
import Network.Protocol.Http
import Network.Salvia.Core.Config
import Network.Socket
import System.IO
import qualified Data.ByteString.Lazy as B

-- | The `HttpM' type class indicates is parametrized with the directon
-- (`Request' or `Response') for which the implementation should be able to
-- supply and modify the values. The `http` method allow for running arbitrary
-- state computations over the request or response objects.

class (Applicative m, Monad m) => HttpM dir m where
  http :: State (Http dir) a -> m a

-- | Stub request and response used to fill in type level gaps for message
-- directions.

forRequest :: Request
forRequest = undefined

forResponse :: Response
forResponse = undefined

-- | Type class alias indicating an HttpM instance for both requests and
-- responses.

class (HttpM Request m, HttpM Response m) => HttpM' m
instance (HttpM Request m, HttpM Response m) => HttpM' m

-- | Direction specific aliases for the `http' method.

request :: HttpM Request m => State (Http Request) a -> m a
request = http

response :: HttpM Response m => State (Http Response) a -> m a
response = http

-- | The `PeerM` type class allows access to peer (the other endpoint of the
-- connection) specific information like the socket and asscociated file handle
-- and the socket address.

class (Applicative m, Monad m) => PeerM m where
  rawSock :: m Socket
  sock    :: m Handle
  peer    :: m SockAddr

type SendAction = (Socket, Handle) -> IO ()

-- | The `QueueM' type class allows for queing actions for sending data values
-- over the wire. Using a queue for collecting send actions instead of directly
-- sending values over the socket allows for a more modular client or server
-- layout.

class (Applicative m, Monad m) => QueueM m where
  enqueue  :: SendAction -> m ()
  dequeue  :: m (Maybe SendAction)

  -- TODO:  queue and dequeue are probably enough.
  sendStr  :: String                                   -> m ()
  sendBs   :: B.ByteString                             -> m ()
  spoolStr :: (String       -> String)       -> Handle -> m ()
  spoolBs  :: (B.ByteString -> B.ByteString) -> Handle -> m ()

-- | The `FlushM' type class can be used to flush the message headers and the
-- message body directly over the wire to the other endpoint.

class (Applicative m, Monad m) => FlushM dir m where
  flushHeaders :: dir -> m ()
  flushQueue   :: dir -> m ()

class (Applicative m, Monad m) => BodyM dir m where
  body :: dir -> m (Maybe B.ByteString)

-- | The `ServerM' type class can be used to acesss the static server
-- configuration.

class (Applicative m, Monad m) => ServerM m where
  server :: m Config

-- | The `ClientM' type class can be used to acesss the static client
-- configuration. Unit for now.

class (Applicative m, Monad m) => ClientM m where
  client :: m ()

-- | The `PayloadM' type class provides access to the server payload. The
-- payload can be an arbitrary piece of data that gets shared between all the
-- handlers. Can be used to implement sessions.

class (Applicative m, Monad m) => PayloadM m p where
  payload :: State p a -> m a

