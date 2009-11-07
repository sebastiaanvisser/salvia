module Network.Salvia.Handler.Body
{-( hRawRequestBody
, hRawResponseBody
, hRawBody
, hRequestBody
, hResponseBody
, hBody
, hRequestParameters
, hResponseParameters
, hParameters
)-}
where

import Control.Applicative
import Control.Monad.State hiding (get)
import Data.Char
import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Uri
import Data.Text.Encoding
import Network.Salvia.Core.Aspects
import Data.Text (Text, unpack)
-- import qualified Data.Text as T
import qualified Data.ByteString as B

-- | Lookup an Data.Text encoding function from a named identifier, currently
-- identifies "utf-8", "utf-16", "utf-32" with possible little/big endian
-- postfixes ("le", "be"). The comparision is quite fuzzy so, for example, both
-- "UTF16le" and "utf-16-LE" will be mapped to the same decoder.

encodingFromName :: String -> Maybe (B.ByteString -> Text)
encodingFromName s = k `lookup`
  [ ("utf8",    decodeUtf8)
  , ("utf16",   decodeUtf16LE)
  , ("utf16le", decodeUtf16LE)
  , ("utf16be", decodeUtf16BE)
  , ("utf32",   decodeUtf32LE)
  , ("utf32le", decodeUtf32LE)
  , ("utf32be", decodeUtf32BE)
  ] where k = map toLower (filter (\c -> isAlpha c || isDigit c) s)

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

hRawBody :: forall m d. (MonadIO m, SockM m, HttpM d m) => d -> m (Maybe B.ByteString)
hRawBody _ =
  do let h = http :: State (Http d) a -> m a
     con <- h (getM connection)
     kpa <- h (getM keepAlive)
     len <- h (getM contentLength)
     s   <- sock
     liftIO $
       case (con, kpa :: Maybe Integer, len :: Maybe Integer) of
         (_, _,       Just n)                           -> Just <$> B.hGet s (fromIntegral n)
         (k, Nothing, Nothing) | k /= Just "keep-alive" -> Just <$> B.hGetContents s
         _                                              -> return Nothing

-- | Like `hRawBody' but specifically for `Http' `Request's.

hRawRequestBody :: BodyM Request m => m (Maybe B.ByteString)
hRawRequestBody = body forRequest

-- | Like `hRawBody' but specifically for `Http' `Request's.

hRawResponseBody :: BodyM Response m => m (Maybe B.ByteString)
hRawResponseBody = body forResponse

{- |
Like the `hRawBody' but is will handle proper decoding based on the charset
part of the `contentType' header line. When a valid encoding is found in the
`Http' message it will be decoded with using the encodings package. The default
encoding supplied as the function's argument can be used to specify what
encoding to use in the absence of a proper encoding in the HTTP message itself.
-}

hBody :: forall m d. (MonadIO m, BodyM d m, HttpM d m) => d -> String -> m (Maybe Text)
hBody d def = 
  do let h = http :: State (Http d) a -> m a
     c <- body d
     e <- (>>= snd) <$> h (getM contentType) :: m (Maybe String)
     return $
       case (e >>= encodingFromName, encodingFromName def) of
         (Just enc, _) -> fmap enc c
         (_, Just enc) -> fmap enc c
         (_, _)        -> error "hBody: wrong default encoding specified"

-- | Like `hBody' but specifically for `HTTP' `Request's.

hRequestBody :: (MonadIO m, BodyM Request m, HttpM Request m) => String -> m (Maybe Text)
hRequestBody = hBody forRequest

-- | Like `hBody' but specifically for `HTTP' `Response's.

hResponseBody :: (MonadIO m, BodyM Response m, HttpM Response m) => String -> m (Maybe Text)
hResponseBody = hBody forResponse

{- |
Try to parse the message body, as a result of `hBody', as URI encoded `POST`
parameters. Returns as a URI `Parameter' type or nothing when parsing fails.
-}

hParameters :: (MonadIO m, BodyM d m, HttpM d m) => d -> String -> m (Maybe Parameters)
hParameters d def = fmap (fw params . unpack) <$> hBody d def

-- | Like `hParameters' but specifically for `HTTP' `Request's.

hRequestParameters :: (MonadIO m, BodyM Request m, HttpM Request m) => String -> m (Maybe Parameters)
hRequestParameters = hParameters forRequest

-- | Like `hParameters' but specifically for `HTTP' `Response's.

hResponseParameters :: (MonadIO m, BodyM Response m, HttpM Response m) => String -> m (Maybe Parameters)
hResponseParameters = hParameters forResponse

