{-# LANGUAGE TemplateHaskell #-}
module Network.Salvia.Core.Context
  ( SendAction
  , SendQueue
  , Context (..)
  , mkContext

  , config
  , request
  , response
  , sock
  , address
  , queue

  ) where

import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Core.Config
import Network.Socket (SockAddr)
import System.IO

-------- single request/response context --------------------------------------

-- | A send action is some thing that works on an IO handle.
type SendAction = Handle -> IO ()

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

data Context =
  Context {
    _config   :: HttpdConfig -- ^ The HTTP server configuration.
  , _request  :: Message     -- ^ The HTTP request header.
  , _response :: Message     -- ^ The HTTP response header.
  , _sock     :: Handle      -- ^ The socket handle for the connection with the client.
  , _address  :: SockAddr    -- ^ The client addres.
  , _queue    :: SendQueue   -- ^ The queue of send actions.
  }

$(mkLabels [''Context])

{- | The queue containing all send actions. -}
queue :: Label Context SendQueue

{- | The client address.  -}
address :: Label Context SockAddr

{- | The socket to the client. -}
sock :: Label Context Handle

{- |
The server response. Using the appropriate handler the response can be sent to
the client after processing the request.
-}

response :: Label Context Message

{- |
The client request. This request is initially empty and only available after
the message has been parsed by the appropriate handler.
-}

request :: Label Context Message

{- |
The global server configuration. Modifying this has no effect on consecutive
requests.
-}

config :: Label Context HttpdConfig

{- |
Create and default server context with the specified server configuration,
client address and socket.
-}

mkContext :: HttpdConfig -> SockAddr -> Handle -> Context
mkContext c a s = Context {
    _config   = c
  , _request  = emptyRequest
  , _response = emptyResponse  -- 200 OK, by default.
  , _sock     = s
  , _address  = a
  , _queue    = []
  }

