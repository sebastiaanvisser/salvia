{-# LANGUAGE TemplateHaskell #-}
module Network.Salvia.Core.Context where

import Data.Record.Label
import Network.Protocol.Http
import Network.Socket
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

type SendQueue = [SendAction]

{- |
A handler context contains all the information needed by the request handlers
to perform their task and to set up a proper response. All the fields in the
context are accessible using the read/write labels defined below.
-}

data Context c p = Context
  { _cConfig     :: c
  , _cPayload    :: p
  , _cRequest    :: Http Request
  , _cResponse   :: Http Response
  , _cRawSock    :: Socket
  , _cSock       :: Handle
  , _cClientAddr :: SockAddr
  , _cServerAddr :: SockAddr
  , _cQueue      :: SendQueue
  }

$(mkLabels [''Context])

-- | The client or server configuration.
cConfig :: Context c p :-> c

-- | Connection wide payload.
cPayload :: Context c p :-> p

-- | The HTTP request header.
cRequest :: Context c p :-> Http Request

-- | The HTTP response header.
cResponse :: Context c p :-> Http Response

-- | Raw socket for connection to the other peer.
cRawSock :: Context c p :-> Socket

-- | Socket file descriptor for connection to the ohter peer.
cSock :: Context c p :-> Handle

-- | Client address.
cClientAddr :: Context c p :-> SockAddr

-- | Server address.
cServerAddr :: Context c p :-> SockAddr

-- | The queue of send actions.
cQueue :: Context c p :-> SendQueue

{- |
Create and default server context with the specified server configuration,
client address and socket.
-}

mkContext :: c -> p -> SockAddr -> SockAddr -> Socket -> Handle -> Context c p
mkContext c p ca sa r s =
  Context
    { _cConfig     = c
    , _cPayload    = p
    , _cRequest    = emptyRequest
    , _cResponse   = emptyResponse  -- 200 OK, by default.
    , _cRawSock    = r
    , _cSock       = s
    , _cClientAddr = ca
    , _cServerAddr = sa
    , _cQueue      = []
    }

-- todo: make peerInfo a Maybe.
emptyContext :: Context () ()
emptyContext = mkContext () ()
  (error "emptyContext: no rawSock available")
  (error "emptyContext: no sock available")
  (error "emptyContext: no client address available")
  (error "emptyContext: no server address available")

