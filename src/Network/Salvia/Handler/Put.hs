{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.Put
( hPutFileSystem
, hPutResource
, hStore
)
where

import Control.Monad.State
import Network.Protocol.Http
import Network.Salvia.Interface
import Network.Salvia.Handler.Body
import Network.Salvia.Handler.Directory
import Network.Salvia.Handler.Error
import Network.Salvia.Handler.File
import Network.Salvia.Handler.FileSystem
import Network.Salvia.Handler.Method
import System.IO
import qualified Data.ByteString.Lazy as B

{- |
Create a browseable filesystem handler (like `hFileSystem') but make all files
writeable by a `PUT' request. Files that do not exists will be created as long
as the directory in which they will be created exists.
-}

hPutFileSystem :: (MonadIO m, HttpM' m, SendM m, BodyM Request m) => FilePath -> m ()
hPutFileSystem = hFileTypeDispatcher hDirectoryResource (hPutResource hFileResource)

{- |
Invokes the `hStore' handler when the request is a `PUT' request and invokes
the fallback handler otherwiser.
-}

hPutResource
  :: (MonadIO m, BodyM Request m, HttpM' m, SendM m)
  => (FilePath -> m ()) -> FilePath -> m ()
hPutResource def fp = hMethod PUT (hStore fp) (def fp)

{- |
This handler takes a FilePath and will try to store the entire request body in
that file. When the request body could for some reason not be fetch a
`BadRequest' error response will be created. When an IO error occurs the
`hIOError' function is used to setup an apropriate response.
-}

hStore
  :: (MonadIO m, BodyM Request m, HttpM Response m, SendM m)
  => FilePath -> m ()
hStore name =
  do b <- hRawRequestBody
     hSafeIO
       (withBinaryFile name WriteMode (flip B.hPut b))
       (const (hCustomError OK "Document stored."))

