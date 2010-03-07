{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.File
( hFile
, hFileResource
, fileMime
, hFileFilter
, hFileResourceFilter
, hResource
, hUri
)
where

import Control.Category
import Control.Monad.State hiding (get)
import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Mime
import Network.Protocol.Uri
import Network.Salvia.Core.Aspects
import Network.Salvia.Handler.Error
import Prelude hiding ((.), id)
import System.IO

{- |
Serve a file from the filesystem indicated by the specified filepath. When
there is some kind of `IOError` the `hSafeIO` function will be used to produce a
corresponding error response. The `contentType` will be the mime-type based on
the filename extension using the `mimetype` function. The `contentLength` will
be set the file's size.
-}

hFileResource :: (MonadIO m, HttpM Response m, SendM m) => FilePath -> m ()
hFileResource file =
  hSafeIO (openBinaryFile file ReadMode) $ \fd ->
    do fs <- liftIO (hFileSize fd)
       response $
         do contentType   =: Just (fileMime file, Nothing)
            contentLength =: Just fs
            status        =: OK
       spool fd

fileMime :: FilePath -> Mime
fileMime file =
    maybe defaultMime id
  $ (either (const Nothing) Just (parseUri file)
  >>= mimetype . get path)

{- |
Like the `hFileResource` handler, but with a custom filter over the content.
This function will assume the content is an UTF-8 encoded text file. Because of
the possibly unpredictable behavior of the filter, no `contentLength` header
will be set using this handler.
-}

hFileResourceFilter :: (MonadIO m, HttpM Response m, SendM m) => (String -> String) -> FilePath -> m ()
hFileResourceFilter f file =
  hSafeIO (openFile file ReadMode) $ \fd ->
    do liftIO (hSetEncoding fd utf8)
       response $
         do contentType =: Just (fileMime file, Just "utf-8")
            status      =: OK
       spoolWith f fd

{- |
Turn a handler that is parametrized by a file resources into a regular handler
that utilizes the path part of the request URI as the resource identifier.
-}

hResource :: HttpM Request m => (FilePath -> m a) -> m a
hResource rh = request (getM (path . asUri)) >>= rh

{- |
Turn a handler that is parametrized by a URI into a regular handler that
utilizes the request URI as the resource identifier.
-}

hUri :: HttpM Request m => (Uri -> m a) -> m a
hUri rh = request (getM asUri) >>= rh

-- | Like `hFileResource` but uses the path of the current request URI.

hFile :: (MonadIO m, HttpM' m, SendM m) => m ()
hFile = hResource hFileResource

-- | Like `hFileResourceFilter` but uses the path of the current request URI.

hFileFilter :: (MonadIO m, HttpM' m, SendM m) => (String -> String) -> m ()
hFileFilter = hResource . hFileResourceFilter

