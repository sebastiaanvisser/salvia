module Network.Salvia.Handler.Directory (
    hDirectory
  , hDirectoryResource
  ) where

import Control.Monad.State
import Data.List (sort)
import Data.Record.Label
import Misc.Misc (bool)
import Network.Protocol.Http
import Network.Protocol.Uri (path)
import Network.Salvia.Handler.File (hResource)
import Network.Salvia.Handler.Redirect
import Network.Salvia.Httpd
import System.Directory (doesDirectoryExist, getDirectoryContents)

{- |
Serve a simple HTML directory listing for the specified directory on the
filesystem.
-}

hDirectoryResource :: (MonadIO m, Request m, Response m, Send m) => FilePath -> m ()
hDirectoryResource dirName =
  do u <- request (getM uri)
     let p = lget path u
     if (null p) || last p /= '/'
      then hRedirect (lmod path (++"/") u)
      else dirHandler dirName

{- |
Like `hDirectoryResource` but uses the path of the current request URI.
-}

hDirectory :: (MonadIO m, Request m, Response m, Send m) => m ()
hDirectory = hResource hDirectoryResource

dirHandler :: (MonadIO m, Request m, Response m, Send m) => FilePath -> m ()
dirHandler dirName =
  do p <- request (getM (path % uri))
     filenames <- liftIO $ getDirectoryContents dirName
     processed <- liftIO $ mapM (processFilename dirName) (sort filenames)
     let b = listing p processed
     response $
       do setM contentType ("text/html", Nothing)
          setM contentLength (Just $ length b)
          setM status OK
     sendStr b

-- Add trailing slash to a directory name.
processFilename :: FilePath -> FilePath -> IO FilePath
processFilename d f = bool (f ++ "/") f `liftM` doesDirectoryExist (d ++ f)

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

