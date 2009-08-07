{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Core.Aspects where

import Control.Applicative
import Control.Monad.State
import Data.Encoding (decodeLazyByteString)
import Data.Encoding.UTF8
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Core.Config
import System.IO
import qualified Network.Socket as S
import qualified Data.ByteString.Lazy as B

-- Todo: figure out better separarion!

class (Applicative m, Monad m) => Config m where
  config  :: m HttpdConfig

class (Applicative m, Monad m) => Client m where
  address :: m S.SockAddr

class (Applicative m, Monad m) => Request m where
  request :: State Message a -> m a

class (Applicative m, Monad m) => Response m where
  response :: State Message a -> m a

class (Applicative m, Monad m) => Socket m where
  rawSock      :: m S.Socket
  sock         :: m Handle

class (Applicative m, Monad m) => Send m where
  sendStr      :: String                                   -> m ()
  sendBs       :: B.ByteString                             -> m ()
  spoolStr     :: (String       -> String)       -> Handle -> m ()
  spoolBs      :: (B.ByteString -> B.ByteString) -> Handle -> m ()
  flushHeaders :: m ()
  flushQueue   :: m ()
  emptyQueue   :: m ()

{- | Reset both the send queue and the generated server response. -}

reset :: (Response m, Send m) => m ()
reset =
  do response (put emptyResponse)
     emptyQueue

sendStrLn :: Send m => String -> m ()
sendStrLn = sendStr . (++"\n")

class (Applicative m, Monad m) => Receive m where
  contents :: m (Maybe B.ByteString)

{- |
Like the `contents' function but decodes the data as UTF-8. Soon, time will
come that decoding will be based upon the requested encoding.
-}

contentsUtf8 :: Receive m => m (Maybe String)
contentsUtf8 = fmap (decodeLazyByteString UTF8) `liftM` contents

{- |
Try to parse the supplied request body as URI encoded `POST` parameters in
UTF-8 encoding. Returns as a URI parameter type or nothing when parsing fails.
-}

uriEncodedPostParamsUTF8 :: Receive m => m (Maybe Parameters)
uriEncodedPostParamsUTF8 =
  (>>= either (const Nothing) Just . parseQueryParams . decode) `liftM` contentsUtf8

