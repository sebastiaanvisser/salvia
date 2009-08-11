{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Network.Salvia.Handler.Contents
  ( hRequestContents
  , hResponseContents
  , hContents
  
  , asASCII
  , asUTF8
  , asParameters
  ) where

import Network.Protocol.Uri
import Data.Encoding.ASCII
import Data.Encoding.UTF8
import Data.Encoding (decodeLazyByteString)
import Control.Monad.State
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Core.Aspects
import qualified Data.ByteString.Lazy as B

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


hRequestContents :: (Request m, MonadIO m, Socket m) => m (Maybe B.ByteString)
hRequestContents  = hContents request

hResponseContents :: (Response m, MonadIO m, Socket m) => m (Maybe B.ByteString)
hResponseContents = hContents response

hContents
  :: (MonadIO m, Socket m)
  => (forall a. State Message a -> m a) -> m (Maybe B.ByteString)
hContents dir =
  do len <- dir (getM contentLength)
     kpa <- dir (getM keepAlive)
--      enc <- dir (getM contentType)
     s   <- sock
     liftIO $
       case (kpa :: Maybe Integer, len :: Maybe Integer) of
         (_,       Just n)  -> liftM Just (B.hGet s (fromIntegral n))
         (Nothing, Nothing) -> liftM Just (B.hGetContents s)
         _                  -> return Nothing


asASCII :: Monad m => m (Maybe B.ByteString) -> m (Maybe String)
asASCII = liftM (fmap (decodeLazyByteString ASCII))

{- |
Like the `contents' function but decodes the data as UTF-8. Soon, time will
come that decoding will be based upon the requested encoding.
-}

asUTF8 :: Monad m => m (Maybe B.ByteString) -> m (Maybe String)
asUTF8 = liftM (fmap (decodeLazyByteString UTF8))

{- |
Try to parse the supplied request body as URI encoded `POST` parameters in
UTF-8 encoding. Returns as a URI parameter type or nothing when parsing fails.
-}

asParameters :: Monad m => m (Maybe String) -> m (Maybe Parameters)
asParameters = liftM (>>= either (const Nothing) Just . parseQueryParams . decode)

