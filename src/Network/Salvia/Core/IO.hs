{-|
This module contains some useful functions for IO over the client socket. Some
functions directly work on the socket and some queue actions into the send
queue. The latter form is more high-level and recommended for common use. This
module is likely to be extended in the future with other useful function that
handle specific message IO.
-}

module Network.Salvia.Core.IO (

  -- * Queing send actions to the send queue.
    send
  , sendStr
  , sendStrLn
  , sendBs

  -- * Queing spool actions to the send queue.
  , spool
  , spoolBs

  -- * Send actions directly using the socket.
  , flushHeaders
  , flushQueue

  -- * Directly manipulate the send queue.
  , emptyQueue
  , reset

  -- * Reading data from the client request.
  , contents
  , contentsUtf8
  , uriEncodedPostParamsUTF8

  ) where

-- TODO: we are mixing two encoding libs. fix.

import Control.Monad.State
import Data.Encoding (decodeLazyByteString)
import Data.Encoding.UTF8
import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Uri (Parameters, parseQueryParams, decode)
import Network.Salvia.Core.Context
import Network.Salvia.Core.Handler
import System.IO
import qualified Data.ByteString.Lazy as B
import qualified System.IO.UTF8 as U

-------------------------------------------------------------------------------

{- |
Queue one potential send action in the send queue. This will not (yet) be sent
over the socket.
-}

send :: SendAction -> Handler ()
send f = modM queue (++[f])

{- | Queue the action of sending one UTF-8 encoded `String` over the socket. -}

sendStr :: String -> Handler ()
sendStr s = send (flip U.hPutStr s)

{- |
Queue the action of sending one UTF-8 encoded `String` with extra linefeed
over the socket.
-}

sendStrLn :: String -> Handler ()
sendStrLn s = send (flip U.hPutStr (s ++ "\n"))

{- | Queue the action of sending one lazy `B.ByteString` over the socket. -}

sendBs :: B.ByteString -> Handler ()
sendBs bs = send (flip B.hPutStr bs)

{- |
Queue spooling the entire contents of a stream to the socket using a UTF-8
encoded `String` based filter.
-}

spool :: (String -> String) -> Handle -> Handler ()
spool f fd = send (\s -> U.hGetContents fd >>= \d -> U.hPutStr s (f d))

{- |
Queue spooling the entire contents of a stream to the socket using a
`B.ByteString` based filter.
-}

spoolBs :: (B.ByteString -> B.ByteString) -> Handle -> Handler ()
spoolBs f fd = send (\s -> B.hGetContents fd >>= \d -> B.hPut s (f d))

{- | Send all the response headers directly over the socket. -}

flushHeaders :: Handler ()
flushHeaders = do
  r <- getM response
  s <- getM sock 
  lift $ hPutStr s (showMessageHeader r)

{- | Apply all send actions successively to the client socket. -}

flushQueue :: Handler ()
flushQueue =
  do h <- getM sock
     q <- getM queue
     liftIO $
       do mapM_ ($ h) q
          hFlush h

{- | Reset the send queue by throwing away all potential send actions. -}

emptyQueue :: Handler ()
emptyQueue = setM queue []

{- | Reset both the send queue and the generated server response. -}

reset :: Handler ()
reset = do
  setM response emptyResponse
  emptyQueue

{- |
First (possibly naive) handler to retreive the client request body as a
`B.ByteString`. This probably does not handle all the quirks that the HTTP
protocol specifies, but it does the job for now. When a 'contentLength' header
field is available only this fixed number of bytes will read from the socket.
When neither the 'keepAlive' and 'contentLength' header fields are available
the entire payload of the request will be read from the socket. This method is
probably only useful in the case of 'PUT' request, because no decoding of
'POST' data is handled.
-}

contents :: Handler (Maybe B.ByteString)
contents = do
  len <- getM (contentLength % request)
  kpa <- getM (keepAlive     % request)
  s   <- getM sock
  lift $
    case (kpa::Maybe Integer, len::Maybe Integer) of
      (_,       Just n)  -> liftM Just (B.hGet s (fromIntegral n))
      (Nothing, Nothing) -> liftM Just (B.hGetContents s)
      _                  -> return Nothing

{- |
Like the `contents' function but decodes the data as UTF-8. Soon, time will
come that decoding will be based upon the requested encoding.
-}

contentsUtf8 :: Handler (Maybe String)
contentsUtf8 = (fmap $ decodeLazyByteString UTF8) `liftM` contents

{- |
Try to parse the supplied request body as URI encoded `POST` parameters in
UTF-8 encoding. Returns as a URI parameter type or nothing when parsing fails.
-}

uriEncodedPostParamsUTF8 :: Handler (Maybe Parameters)
uriEncodedPostParamsUTF8 = liftM (>>= parseQueryParams . decode) contentsUtf8

