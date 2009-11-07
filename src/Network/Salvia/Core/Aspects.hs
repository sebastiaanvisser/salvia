{-# LANGUAGE UndecidableInstances #-}
module Network.Salvia.Core.Aspects where

import Control.Applicative
import Control.Monad.State
import Network.Protocol.Http
import Network.Salvia.Core.Config
import Network.Socket
import System.IO
import Data.ByteString (ByteString)

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

-- | The `SockM` type class allows access to peer (the other endpoint of the
-- connection) specific information like the socket and file handle associated
-- with the socket.

class (Applicative m, Monad m) => SockM m where
  rawSock :: m Socket
  sock    :: m Handle

-- | The `ClientAddressM` type class gives access to socket address of the
-- client part of the connection.

class (Applicative m, Monad m) => ClientAddressM m where
  clientAddress :: m SockAddr

-- | The `ServerAddressM` type class gives access to socket address of the
-- client part of the connection.

class (Applicative m, Monad m) => ServerAddressM m where
  serverAddress :: m SockAddr

-- | Type class alias indicating an instances for both `ClientAddressM' and
-- `ServerAddressM'.

class (ClientAddressM m, ServerAddressM m) => AddressM' m
instance (ClientAddressM m, ServerAddressM m) => AddressM' m

type SendAction = (Socket, Handle) -> IO ()

-- | The `QueueM' type class allows for queing actions for sending data values
-- over the wire. Using a queue for collecting send actions instead of directly
-- sending values over the socket allows for a more modular client or server
-- layout.

class (Applicative m, Monad m) => QueueM m where
  enqueue  :: SendAction -> m ()
  dequeue  :: m (Maybe SendAction)

class (Applicative m, Monad m) => SendM m where

  -- | Enqueue the action of sending one regular Haskell `String' over the wire
  -- to the other endpoint.

  send :: String -> m ()

  -- | Enqueue the action of sending one `ByteString' over the wire to the
  -- other endpoint.

  sendBs :: ByteString -> m ()

  -- | Like the `spool' function but allows a custom filter over the contents.
  -- the wire to the other endpoint.

  spoolWith :: (String -> String) -> Handle -> m ()

  -- | Like the `spoolWith' function but uses a direct `ByteString' filter
  -- which might be more efficient.

  spoolWithBs :: (ByteString -> ByteString) -> Handle -> m ()

-- | Enqueue the action of spooling the entire contents of a file handle over
-- the wire to the other endpoint.

spool :: SendM m => Handle -> m ()
spool = spoolWithBs id

-- | The `FlushM' type class can be used to flush the message headers and the
-- message body directly over the wire to the other endpoint.

class (Applicative m, Monad m) => FlushM dir m where
  flushHeaders :: dir -> m ()
  flushQueue   :: dir -> m ()

class (Applicative m, Monad m) => BodyM dir m where
  body :: dir -> m (Maybe ByteString)

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

class (Applicative m, Monad m) => PayloadM m p | m -> p where
  payload :: State p a -> m a

