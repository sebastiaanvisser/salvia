module Network.Salvia.Handler.FileSystem (
    hFileSystem
  , hFileSystemNoIndexes
  , hFileTypeDispatcher
  ) where

import Control.Monad.State
import Data.Record.Label
import Misc.Misc (bool)
import Network.Protocol.Http
import Network.Protocol.Uri (jail, path)
import Network.Salvia.Handler.Directory
import Network.Salvia.Handler.Error
import Network.Salvia.Handler.File
import Network.Salvia.Httpd
import System.Directory (doesDirectoryExist)

{- |
Dispatch based on file type; regular files or directories. The first handler
specified will be invoked in case the resource to be served is an directory,
the second handler otherwise. The path from the request URI will be appended to
the directory resource specified as a parameter, this new path will be used to
lookup the real resource on the file system. Every request will be jailed in
the specified directory resource to prevent users from requesting arbitrary
parts of the file system.
-}

hFileTypeDispatcher
  :: (MonadIO m, Request m, Response m, Send m)
  => (FilePath -> m ()) -> (FilePath -> m ()) -> FilePath -> m ()
hFileTypeDispatcher hdir hfile dir =
  request (getM (path % asURI)) >>=
    hJailedDispatch dir hdir hfile . (dir ++)

{- |
Serve single directory by combining the `hDirectoryResource` and
`hFileResource` handlers in the `hFileTypeDispatcher`.
-}

hFileSystem :: (MonadIO m, Request m, Response m, Send m) => FilePath -> m ()
hFileSystem = hFileTypeDispatcher hDirectoryResource hFileResource

{- |
Serve single directory like `hFileSystem` but do not show directory indices.
Instead of an directory index an `Forbidden` response will be created.
-}

hFileSystemNoIndexes :: (MonadIO m, Request m, Response m, Send m) => FilePath -> m ()
hFileSystemNoIndexes = hFileTypeDispatcher (const $ hError Forbidden) hFileResource

hJailedDispatch
  :: (MonadIO m, Request m, Response m, Send m)
  => FilePath -> (FilePath -> m ()) -> (FilePath -> m ()) -> FilePath -> m () 
hJailedDispatch dir hdir hfile file = do
  case jail dir file of
    Nothing -> hError Forbidden
    Just f  -> bool (hdir file) (hfile file)
           =<< liftIO (doesDirectoryExist f) 

