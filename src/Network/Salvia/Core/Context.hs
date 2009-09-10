{-# LANGUAGE TemplateHaskell #-}
module Network.Salvia.Core.Context
{-  ( SendAction
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
  )-}
where

import Control.Category
import Data.Record.Label
import Network.Protocol.Http
import Network.Socket (SockAddr, Socket)
import Prelude hiding ((.), id)
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

data PeerInfo = PeerInfo
  { __rawSock :: Socket
  , __sock    :: Handle
  , __peer    :: SockAddr
  }

$(mkLabels [''PeerInfo])

_rawSock :: PeerInfo :-> Socket
_sock    :: PeerInfo :-> Handle
_peer    :: PeerInfo :-> SockAddr

{- |
A handler context contains all the information needed by the request handlers
to perform their task and to set up a proper response. All the fields in the
context are accessible using the read/write labels defined below.
-}


data Context c p = Context
  { _config   :: c             -- ^ The client or server configuration.
  , _payload  :: p             -- ^ Connection wide payload.
  , _request  :: Http Request  -- ^ The HTTP request header.
  , _response :: Http Response -- ^ The HTTP response header.
  , _peerInfo :: PeerInfo      -- ^ The raw socket for the connection with the other endpoint. 
  , _queue    :: SendQueue     -- ^ The queue of send actions.
  }

$(mkLabels [''Context])

config   :: Context c p :-> c
payload  :: Context c p :-> p
queue    :: Context c p :-> SendQueue
peerInfo :: Context c p :-> PeerInfo
request  :: Context c p :-> Http Request
response :: Context c p :-> Http Response

rawSock :: Context c p :-> Socket
rawSock = _rawSock . peerInfo

sock :: Context c p :-> Handle
sock = _sock . peerInfo

peer :: Context c p :-> SockAddr
peer = _peer . peerInfo

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
    , _peerInfo = PeerInfo r s a
    , _queue    = []
    }

