module Network.Salvia.Handler.Put (hPut) where

import Control.Monad.State
import Network.Protocol.Http
import Network.Salvia.Handler.Error
import Network.Salvia.Core.Aspects
import System.IO
import qualified Data.ByteString.Lazy as B

{- |
First naive handler for the HTTP `PUT` request. This probably does not handle
all the quirks that the HTTP protocol specifies, but it does the job for now.
When a 'contentLength' header field is available only this fixed number of
bytes will be spooled from socket to the resource. When both the `keepAlive'
and 'contentLength' header fields are not available the entire payload of the
request is spooled to the resource.
-}

hPut :: (MonadIO m, Response m, Send m, Contents m) => FilePath -> m ()
hPut name =
  hSafeIO (openBinaryFile name WriteMode)
    $ (contents >>=) . maybe putError . putOk
  where
    putError   = hError NotImplemented
    putOk fd c = liftIO (B.hPut fd c >> hClose fd)

