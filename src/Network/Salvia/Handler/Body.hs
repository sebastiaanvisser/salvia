module Network.Salvia.Handler.Body   {- too doc, rename body -}
  ( hRawRequestBody
  , hRawResponseBody
  , hRawBody
  
  , hRequestBody
  , hResponseBody
  , hBody

  , hRequestParameters
  , hResponseParameters
  , hParameters
  )
where

import Data.Maybe
import Data.Encoding hiding (decode)
import Control.Applicative
import Control.Monad.State
import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Core.Aspects
import qualified Data.Encoding as E
import qualified Data.ByteString.Lazy as B

{- |
First (possibly naive) handler to retreive the client request or server
response body as a raw lazy `B.ByteString`. This probably does not handle all
the quirks that the HTTP protocol specifies, but it does the job for now. When
a 'contentLength' header field is available only this fixed number of bytes
will read from the socket.  When neither the 'keepAlive' and 'contentLength'
header fields are available the entire payload of the request will be read from
the socket. The function is parametrized with a the direction of the HTTP
message, client request or server response.
-}

hRawBody :: forall m d. (MonadIO m, SocketM m, HttpM d m) => d -> m (Maybe B.ByteString)
hRawBody _ =
  do let h = http :: State (HTTP d) a -> m a
     len <- h (getM contentLength)
     kpa <- h (getM keepAlive)
     s   <- sock
     liftIO $
       case (kpa :: Maybe Integer, len :: Maybe Integer) of
         (_,       Just n)  -> Just <$> B.hGet s (fromIntegral n)
         (Nothing, Nothing) -> Just <$> B.hGetContents s
         _                  -> return Nothing

-- | Like `hRawBody' but specifically for `HTTP' `Request's.

hRawRequestBody :: BodyM Request m => m (Maybe B.ByteString)
hRawRequestBody = body forRequest

-- | Like `hRawBody' but specifically for `HTTP' `Request's.

hRawResponseBody :: BodyM Response m => m (Maybe B.ByteString)
hRawResponseBody = body forResponse

{- |
Like the `hRawBody' but is will handle proper decoding based on the charset
part of the `contentType' header line. When a valid encoding is found in the
`HTTP' message it will be decoded with using the encodings package. The default
encoding supplied as the function's argument can be used to specify what
encoding to use in the absence of a proper encoding in the HTTP message itself.
-}

hBody :: forall m d. (MonadIO m, BodyM d m, HttpM d m) => d -> String -> m (Maybe String)
hBody d def = 
  do let h = http :: State (HTTP d) a -> m a
     c <- body d
     e <- h (getM contentType)
     return (decodeWith def <$> (e >>= snd) <*> c)

-- | Like `hBody' but specifically for `HTTP' `Request's.

hRequestBody :: (MonadIO m, BodyM Request m, HttpM Request m) => String -> m (Maybe String)
hRequestBody = hBody forRequest

-- | Like `hBody' but specifically for `HTTP' `Response's.

hResponseBody :: (MonadIO m, BodyM Response m, HttpM Response m) => String -> m (Maybe String)
hResponseBody = hBody forResponse

{- |
Try to parse the supplied request, as a result of `hBody', as URI encoded
`POST` parameters. Returns as a URI `Parameter' type or nothing when parsing
fails.
-}

hParameters :: (MonadIO m, BodyM d m, HttpM d m) => d -> String -> m (Maybe Parameters)
hParameters d def =
  (>>= either (const Nothing) Just . parseQueryParams . decode) <$> hBody d def

hRequestParameters :: (MonadIO m, BodyM Request m, HttpM Request m) => String -> m (Maybe Parameters)
hRequestParameters = hParameters forRequest

hResponseParameters :: (MonadIO m, BodyM Response m, HttpM Response m) => String -> m (Maybe Parameters)
hResponseParameters = hParameters forResponse

decodeWith :: String -> String -> B.ByteString -> String
decodeWith def enc = 
  decodeLazyByteString (fromMaybe (encodingFromString def) (encodingFromStringExplicit enc))

