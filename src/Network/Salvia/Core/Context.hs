{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Network.Salvia.Core.Context
  ( SendAction
  , SendQueue

  , Context (..)

  , config
  , payload
  , request
  , response
  , rawSock
  , sock
  , peer
  , queue

  , mkContext

  ) where

import Data.Record.Label
import Network.Protocol.Http
import Network.Socket (SockAddr, Socket)
import System.IO

-- | A send action is some thing that works on a socket or handle.
type SendAction = (Socket, Handle) -> IO ()

{- |
The send queue is an abstraction to make sure all data that belongs to the
message body is sent after the response headers have been sent.  Instead of
sending data to client directly over the socket from the context it is
preferable to queue send actions in the context's send queue. The entire send
queue can be flushed to the client at once after the HTTP headers have been
sent at the end of a request handler.
-}

type SendQueue  = [SendAction]

{- |
A handler context contains all the information needed by the request handlers
to perform their task and to set up a proper response. All the fields in the
context are accessible using the read/write labels defined below.
-}

data Context c p = Context
  { _config   :: c             -- ^ The client or server configuration.
  , _payload  :: p             -- ^ Connection wide payload.
                               
  , _request  :: HTTP Request  -- ^ The HTTP request header.
  , _response :: HTTP Response -- ^ The HTTP response header.
                               
  , _rawSock  :: Socket        -- ^ The raw socket for the connection with the other endpoint. 
  , _sock     :: Handle        -- ^ The socket handle for the connection with the other endpoint.
  , _peer     :: SockAddr      -- ^ The address of the other endpoint.
                               
  , _queue    :: SendQueue     -- ^ The queue of send actions.
  }

$(mkLabels [''Context])

config   :: Context c p :-> c
payload  :: Context c p :-> p
queue    :: Context c p :-> SendQueue
peer     :: Context c p :-> SockAddr
rawSock  :: Context c p :-> Socket
sock     :: Context c p :-> Handle
request  :: Context c p :-> HTTP Request
response :: Context c p :-> HTTP Response

{- |
Create and default server context with the specified server configuration,
client address and socket.
-}

mkContext :: c -> p -> SockAddr -> Socket -> Handle -> Context c p
mkContext c p a r s =
  Context
    { _config   = c
    , _payload  = p
    , _request  = emptyRequest
    , _response = emptyResponse  -- 200 OK, by default.
    , _rawSock  = r
    , _sock     = s
    , _peer     = a
    , _queue    = []
    }

