{-# LANGUAGE
    UndecidableInstances
  , TypeOperators
  , MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleContexts
  , FlexibleInstances
  , TypeFamilies
  , IncoherentInstances
  #-}
module Network.Salvia.Interface where

import Control.Concurrent.STM
import Control.Applicative
import Control.Category
import Control.Monad.State hiding (get)
import Data.ByteString.Lazy (ByteString)
import Data.Record.Label
import Network.Protocol.Http
import Network.Socket
import Prelude hiding ((.), id)
import System.IO

-- todo: comment

class ForkM n m where
  forkM :: m a -> m (n a)

-- | The `HttpM' type class indicates is parametrized with the directon
-- (`Request' or `Response') for which the implementation should be able to
-- supply and modify the values. The `http` method allow for running arbitrary
-- state computations over the request or response objects.

class (Applicative m, Monad m) => HttpM dir m where
  http :: State (Http dir) a -> m a

class (Applicative m, Monad m) => RawHttpM dir m where
  rawHttp :: State (Http dir) a -> m a

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

class (RawHttpM Request m, RawHttpM Response m) => RawHttpM' m
instance (RawHttpM Request m, RawHttpM Response m) => RawHttpM' m

-- | Direction specific aliases for the `http' method.

request :: HttpM Request m => State (Http Request) a -> m a
request = http

response :: HttpM Response m => State (Http Response) a -> m a
response = http

rawRequest :: RawHttpM Request m => State (Http Request) a -> m a
rawRequest = rawHttp

rawResponse :: RawHttpM Response m => State (Http Response) a -> m a
rawResponse = rawHttp

-- | The `SockM` type class allows access to peer (the other endpoint of the
-- connection) specific information like the socket and file handle associated
-- with the socket.

class (Applicative m, Monad m) => SockM m where
  socket :: m Socket
  handle :: m Handle

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

{- |
The send queue is an abstraction to make sure all data that belongs to the
message body is sent after the response headers have been sent.  Instead of
sending data to client directly over the socket from the context it is
preferable to queue send actions in the context's send queue. The entire send
queue can be flushed to the client at once after the HTTP headers have been
sent at the end of a request handler.
-}

type SendQueue = [SendAction]

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
  body :: dir -> m ByteString

-- | The `ServerM' type class can be used to acesss the static server
-- configuration like the address/port combination the server listens on and
-- the related hostname.

class (Applicative m, Monad m) => ServerM m where
  host   :: m String
  admin  :: m String
  listen :: m [SockAddr]

-- | The `ClientM' type class can be used to acesss the static client
-- configuration. Unit for now.

class (Applicative m, Monad m) => ClientM m where
  client :: m ()

-- | The `PayloadM' type class provides access to the server payload. The
-- payload can be an arbitrary piece of data that gets shared between all the
-- handlers. Can be used to implement sessions and such. Heterogeneous lists
-- implemented as right associated nested tuples can be used to store multiple
-- pieces of information and still let individual handlers pick out the right
-- thing they need. Picking the right pieces of information from the payload
-- can be done with the `select' function from the `Contains' type class.

class (Applicative m, Monad m, Contains p (TVar q)) => PayloadM p q m | m -> p where
  payload :: State q a -> m a

infixr 5 &
(&) :: a -> b -> (a, b)
(&) a b = (a, b)

class Contains a b where
  select :: a :-> b

instance (a ~ a') => Contains a a' where
  select = id

instance Contains (a, c) a where
  select = label fst (\a (_, b) -> (a, b))

instance (b ~ b', Contains a b') => Contains (c, a) b' where
  select = select . label snd (\b (a, _) -> (a, b))

