{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
module Network.Salvia.Handler.Body
( hRawRequestBody
, hRawResponseBody
, hRawBody

, hRequestBodyText
, hResponseBodyText
, hBodyText

, hRequestBodyStringUTF8
, hResponseBodyStringUTF8
, hBodyStringUTF8

, hRequestParameters
, hResponseParameters
, hParameters
)
where

import Control.Applicative
import Control.Monad.State hiding (get)
import Data.Char
import Data.Record.Label
import Data.Text.Lazy (Text, unpack)
import Data.Text.Lazy.Encoding
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Interface
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as U

-- | Lookup an Data.Text encoding function from a named identifier, currently
-- identifies "utf-8", "utf-16", "utf-32" with possible little/big endian
-- postfixes ("le", "be"). The comparision is quite fuzzy so, for example, both
-- "UTF16le" and "utf-16-LE" will be mapped to the same decoder.

encodingFromName :: String -> Maybe (B.ByteString -> Text)
encodingFromName s = k `lookup`
  [ ("utf8",    decodeUtf8)
--   , ("utf16",   decodeUtf16LE)
--   , ("utf16le", decodeUtf16LE)
--   , ("utf16be", decodeUtf16BE)
--   , ("utf32",   decodeUtf32LE)
--   , ("utf32le", decodeUtf32LE)
--   , ("utf32be", decodeUtf32BE)
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

hRawBody :: forall m d. (MonadIO m, HandleM m, HttpM d m) => d -> m B.ByteString
hRawBody _ =
  do let h = http :: State (Http d) a -> m a
     con <- h (getM connection)
     kpa <- h (getM keepAlive)
     len <- h (getM contentLength)
     s   <- handle
     liftIO $
       case (con, kpa :: Maybe Integer, len :: Maybe Integer) of
         (_, _,       Just n)                           -> B.hGet s (fromIntegral n)
         (k, Nothing, Nothing) | k /= Just "keep-alive" -> B.hGetContents s
         _                                              -> return B.empty

-- | Like `hRawBody' but specifically for `Http' `Request's.

hRawRequestBody :: BodyM Request m => m B.ByteString
hRawRequestBody = body forRequest

-- | Like `hRawBody' but specifically for `Http' `Request's.

hRawResponseBody :: BodyM Response m => m B.ByteString
hRawResponseBody = body forResponse

{- |
Like the `hRawBody' but is will handle proper decoding based on the charset
part of the `contentType' header line. When a valid encoding is found in the
`Http' message it will be decoded with using the encodings package. The default
encoding supplied as the function's argument can be used to specify what
encoding to use in the absence of a proper encoding in the HTTP message itself.
-}

hBodyText :: forall m dir. (BodyM dir m, HttpM dir m) => dir -> String -> m Text
hBodyText d def = 
  do let h = http :: State (Http dir) a -> m a
     c <- body d
     e <- (>>= snd) <$> h (getM contentType) :: m (Maybe String)
     return $
       case (e >>= encodingFromName, encodingFromName def) of
         (Just enc, _) -> enc c
         (_, Just enc) -> enc c
         (_, _)        -> error "hBodyText: wrong default encoding specified"

-- | Like `hBodyText' but specifically for `Http' `Request's.

hRequestBodyText :: (BodyM Request m, HttpM Request m) => String -> m Text
hRequestBodyText = hBodyText forRequest

-- | Like `hBodyText' but specifically for `Http' `Response's.

hResponseBodyText :: (BodyM Response m, HttpM Response m) => String -> m Text
hResponseBodyText = hBodyText forResponse

-- | Like the `hRawBody' but decodes it as UTF-8 to a `String'.

hBodyStringUTF8 :: BodyM dir m => dir -> m String
hBodyStringUTF8 d = U.toString <$> body d

-- | Like `hBodyStringUTF8' but specifically for `Http' `Request's.

hRequestBodyStringUTF8 :: BodyM Request m => m String
hRequestBodyStringUTF8 = hBodyStringUTF8 forRequest

-- | Like `hBodyStringUTF8' but specifically for `Http' `Response's.

hResponseBodyStringUTF8 :: BodyM Response m => m String
hResponseBodyStringUTF8 = hBodyStringUTF8 forResponse

{- |
Try to parse the message body, as a result of `hBodyText', as URI encoded `POST`
parameters. Returns as a URI `Parameter' type or nothing when parsing fails.
-}

hParameters :: (BodyM d m, HttpM d m) => d -> String -> m Parameters
hParameters d def = fw params . unpack <$> hBodyText d def

-- | Like `hParameters' but specifically for `HTTP' `Request's.

hRequestParameters :: (BodyM Request m, HttpM Request m) => String -> m Parameters
hRequestParameters = hParameters forRequest

-- | Like `hParameters' but specifically for `HTTP' `Response's.

hResponseParameters :: (BodyM Response m, HttpM Response m) => String -> m Parameters
hResponseParameters = hParameters forResponse

