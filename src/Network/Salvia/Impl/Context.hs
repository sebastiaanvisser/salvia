{-# LANGUAGE TemplateHaskell, DeriveFunctor, TypeOperators #-}
module Network.Salvia.Impl.Context where

import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Interface
import Network.Socket
import System.IO

{- |
A generic handler context that contains all the information needed by the
request handlers to perform their task and to set up a proper response. All the
fields in the context are accessible using the first class labels defined
below. 
-}

data Context p = Context
  { _cServerHost  :: String
  , _cAdminMail   :: String
  , _cListenOn    :: [SockAddr]
  , _cRequest     :: Http Request
  , _cResponse    :: Http Response
  , _cRawRequest  :: Http Request
  , _cRawResponse :: Http Response
  , _cSocket      :: Socket
  , _cHandle      :: Handle
  , _cClientAddr  :: SockAddr
  , _cServerAddr  :: SockAddr
  , _cQueue       :: SendQueue
  , _cPayload     :: p
  } deriving Functor

$(mkLabels [''Context])

-- | The server hostname.
cServerHost :: Context p :-> String

-- | The mail address of the server adminstrator.
cAdminMail :: Context p :-> String

-- | The socket address(es) the server is listening on.
cListenOn :: Context p :-> [SockAddr]

-- | Connection wide payload.
cPayload :: Context p :-> p

-- | The HTTP request header.
cRequest :: Context p :-> Http Request

-- | The HTTP response header.
cResponse :: Context p :-> Http Response

-- | The unaltered HTTP request header as received from a client.
cRawRequest :: Context p :-> Http Request

-- | The plain HTTP response header unaffected by local rewriting.
cRawResponse :: Context p :-> Http Response

-- | Raw socket for connection to the peer.
cSocket :: Context p :-> Socket

-- | File descriptor associated with socket for the connection to the peer.
cHandle :: Context p :-> Handle

-- | Client address.
cClientAddr :: Context p :-> SockAddr

-- | Server address.
cServerAddr :: Context p :-> SockAddr

-- | The queue of send actions.
cQueue :: Context p :-> SendQueue

