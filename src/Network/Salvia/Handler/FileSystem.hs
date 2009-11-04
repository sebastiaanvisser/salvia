{- | Serving parts of the local file system. -}
module Network.Salvia.Handler.FileSystem
( hFileSystem
, hFileSystemNoIndexes
, hFileTypeDispatcher
)
where

import Control.Category
import Control.Monad.State
import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Core.Aspects
import Network.Salvia.Handler.Directory
import Network.Salvia.Handler.Error
import Network.Salvia.Handler.File
import Prelude hiding ((.), id)
import System.Directory

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
  :: (MonadIO m, HttpM' m, SendM m)
  => (FilePath -> m ())  -- ^ Handler to invoke in case of directory.
  -> (FilePath -> m ())  -- ^ Handler to invoke in case of regular files.
  -> FilePath            -- ^ Directory to serve.
  -> m ()
hFileTypeDispatcher hdir hfile dir =
  do p <- request $ getM (path . asUri) 
     hJailedDispatch dir hdir hfile (dir /+ p)

{- |
Serve single directory by combining the `hDirectoryResource` and
`hFileResource` handlers in the `hFileTypeDispatcher`.
-}

hFileSystem
  :: (MonadIO m, HttpM' m, SendM m)
  => FilePath  -- ^ Directory to serve.
  -> m ()
hFileSystem = hFileTypeDispatcher hDirectoryResource hFileResource

{- |
Serve single directory like `hFileSystem` but do not show directory indices.
Instead of an directory index an `Forbidden` response will be created.
-}

hFileSystemNoIndexes
  :: (MonadIO m, HttpM' m, SendM m)
  => FilePath  -- ^ Directory to serve.
  -> m ()
hFileSystemNoIndexes = hFileTypeDispatcher (const $ hError Forbidden) hFileResource

-- Helper distpatcher that takes care of jailing the request in the specified
-- file system directory.

hJailedDispatch
  :: (MonadIO m, HttpM' m, SendM m)
  => FilePath -> (FilePath -> m ()) -> (FilePath -> m ()) -> FilePath -> m () 
hJailedDispatch dir hdir hfile file =
  do case jail dir file of
       Nothing -> hError Forbidden
       Just f -> (\b -> (if b then hdir else hfile) file)
                 =<< liftIO (doesDirectoryExist f) 

