{- | Rendering of HTML directory listings. -}
module Network.Salvia.Handler.Directory
  ( hDirectory
  , hDirectoryResource
  )
where

import Control.Applicative
import Control.Category
import Control.Monad.State hiding (get)
import Data.List (sort)
import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Uri (path)
import Network.Salvia.Interface
import Network.Salvia.Handler.File (hResource)
import Network.Salvia.Handler.Redirect
import Prelude hiding ((.), id, mod)
import System.Directory (doesDirectoryExist, getDirectoryContents)

{- |
Serve a simple HTML directory listing for the specified directory on the
filesystem.
-}

hDirectoryResource
  :: (MonadIO m, HttpM' m, SendM m)
  => FilePath  -- ^ Directory to produce a listing for.
  -> m ()
hDirectoryResource dirName =
  do u <- request (getM asUri)
     let p = get path u
     if (null p) || last p /= '/'
       then hRedirect (show $ mod path (++"/") u)
       else dirHandler dirName

{- |
Like `hDirectoryResource` but uses the path from the current request URI.
-}

hDirectory :: (MonadIO m, HttpM' m, SendM m) => m ()
hDirectory = hResource hDirectoryResource

-- Helper function that does all the work.

dirHandler :: (MonadIO m, HttpM' m, SendM m) => FilePath -> m ()
dirHandler dirName =
  do p <- request (getM (path . asUri))
     filenames <- liftIO $ getDirectoryContents dirName
     processed <- liftIO $ mapM (processFilename dirName) (sort filenames)
     let b = listing p processed
     response $
       do contentType   =: Just ("text/html", Nothing)
          contentLength =: Just (length b)
          status        =: OK
     send b

-- Add trailing slash to a directory name.
processFilename :: FilePath -> FilePath -> IO FilePath
processFilename d f =
  (\b -> (if b then (++"/") else id) f) <$> doesDirectoryExist (d ++ f)

-- Turn a list of filenames into HTML directory listing.
listing :: FilePath -> [FilePath] -> String
listing dirName fileNames =
  concat [
    "<html><head><title>Index of "
  , dirName
  , "</title></head><body><h1>Index of "
  , dirName
  , "</h1><ul>"
  , fileNames >>= \f -> concat ["<li><a href='", f, "'>", f, "</a></li>"]
  , "</ul></body></html>"
  ]

