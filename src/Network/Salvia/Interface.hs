{- |
This interface module contains all the basic operations to access the server
context. The interface is just of bunch of type classes that allow access to
the request and response objects. Most type classes allow access to the context
information through lifted state computations. To dig deeper into the context
object you would probably want to use the derived /fclabels/ accessors.

Example 1: To get the entire request object:

> do r <- request get  -- Control.Monad.State.get

Example 2: To get the request URI as a string:

> do r <- request (getM uri) -- getM from Data.Record.Label

Example 3: To get the query parameters and the /User-Agent/ header:

> do request $
>      do q <- getM (queryParams . asUri)  -- composed lenses using the (.) from Control.Category.
>         u <- header "user-agent"
>         return (q, u)

Example 4: To set the /Content-Type/ and response status and send some string.

> do response $
>      do status =: BadRequest    -- the (=:) operator from Data.Record.Label
>         header "content-type" =: "text/plain"
>    send "hello, world"

-}

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
import Control.Concurrent
import Control.Monad.State hiding (get)
import Data.ByteString.Lazy (ByteString)
import Data.Record.Label
import Network.Protocol.Http
import Network.Socket
import Prelude hiding ((.), id)
import System.IO

class MonadIO m => ForkableMonad m where
  fork :: m a -> m ThreadId

-- | The `HttpM' type class indicates is parametrized with the directon
-- (`Request' or `Response') for which the implementation should be able to
-- supply and modify the values. The `http` method allow for running arbitrary
-- state computations over the request or response objects.

class (Applicative m, Monad m) => HttpM dir m where
  http    :: State (Http dir) a -> m a
  rewrite :: (Http dir -> Http dir) -> m a -> m a

-- | The raw HTTP Request and Response objects represent the request and
-- responses as received over the line. These should not be subject to any form
-- of rewriting.

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

-- | The `SocketM` type class allows access to the raw socket.

class (Applicative m, Monad m) => SocketM m where
  socket :: m Socket

-- | The `HandleM` type class allows access to the file handle, probabaly
-- associated with the socket to the peer. There is a separate handle for each
-- of the directions.

class (Applicative m, Monad m) => HandleM m where
  handleIn  :: m Handle
  handleOut :: m Handle

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

data SendAction = SendAction ((Socket, Handle) -> IO ())

instance Show SendAction where
  show _ = "<send action>"

-- | todo: comment:
-- The `QueueM' type class allows for queing actions for sending data values
-- over the wire. Using a queue for collecting send actions instead of directly
-- sending values over the socket allows for a more modular client or server
-- layout.

class (Applicative m, Monad m) => HandleQueueM m where
  enqueueHandle :: (Handle -> IO ()) -> m ()

class (Applicative m, Monad m) => SocketQueueM m where
  enqueueSock :: (Socket -> IO ()) -> m ()

class (Applicative m, Monad m) => QueueM m where
  dequeue :: m (Maybe SendAction)

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
  serverHost   :: m String
  serverAdmin  :: m String
  listen       :: m [SockAddr]

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
  select = lens fst (\a (_, b) -> (a, b))

instance (b ~ b', Contains a b') => Contains (c, a) b' where
  select = select . lens snd (\b (a, _) -> (a, b))

