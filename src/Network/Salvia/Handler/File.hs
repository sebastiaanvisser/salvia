module Network.Salvia.Handler.File (
    hFile
  , hFileResource

  , hFileFilter
  , hFileResourceFilter

  , hResource
  , hUri
  ) where

import Control.Monad.State
import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Mime
import Network.Protocol.Uri
import Network.Salvia.Handler.Error
import Network.Salvia.Httpd
import System.IO

{- |
Serve a file from the filesystem indicated by the specified filepath. When
there is some kind of `IOError` the `hSafeIO` function will be used to produce a
corresponding error response. The `contentType` will be the mime-type based on
the filename extension using the `mimetype` function. The `contentLength` will
be set the file's size.
-}

-- TODO: what to do with encoding?
hFileResource :: (MonadIO m, Response m, Send m) => FilePath -> m ()
hFileResource file = do
  let m = maybe defaultMime id $ (either (const Nothing) Just (parseURI file) >>= mimetype . lget path)
  hSafeIO (openBinaryFile file ReadMode)
    $ \fd -> do
      fs <- liftIO $ hFileSize fd
      response $
        do setM contentType (m, Just "utf-8")
           setM contentLength (Just fs)
           setM status OK
      spoolBs id fd

{- |
Like the `hFileResource` handler, but with a custom filter over the content.
This function will assume the content is an UTF-8 encoded text file. No
`contentLength` header will be set using this handler.
-}

-- TODO: what to do with encoding?
hFileResourceFilter :: (MonadIO m, Response m, Send m) => (String -> String) -> FilePath -> m ()
hFileResourceFilter fFilter file = do  -- TODO... this should be a more general hFilter
  let m = maybe defaultMime id $ (either (const Nothing) Just (parseURI file) >>= mimetype . lget path)
  hSafeIO (openBinaryFile file ReadMode)
    $ \fd -> do
      response $
        do setM contentType (m, Just "utf-8")
           setM status OK
      spoolStr fFilter fd

{- |
Turn a resource handler into a regular handler that utilizes the path part of
the request URI as the resource identifier.
-}

hResource :: Request m => (FilePath -> m a) -> m a
hResource rh = request (getM (path % uri)) >>= rh

{- |
Turn a URI handler into a regular handler that utilizes the request URI as the
resource identifier.
-}

hUri :: Request m => (URI -> m a) -> m a
hUri rh = request (getM uri) >>= rh

{- |
Like `hFileResource` but uses the path of the current request URI.
-}

hFile :: (MonadIO m, Request m, Response m, Send m) => m ()
hFile = hResource hFileResource

{- |
Like `hFileResourceFilter` but uses the path of the current request URI.
-}

hFileFilter :: (MonadIO m, Request m, Response m, Send m) => (String -> String) -> m ()
hFileFilter = hResource . hFileResourceFilter

