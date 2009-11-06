module Network.Salvia.Handler.CGI (hCGI) where

import Control.Applicative
import Control.Category
import Control.Concurrent
import Control.Monad.State
import Data.Char
import Data.List
import Data.List.Split
import Data.Map (toList)
import Data.Record.Label
import Network.Protocol.Http hiding (hostname, server)
import Network.Protocol.Uri
import Network.Salvia.Core.Aspects
import Network.Salvia.Core.Config
import Network.Salvia.Handler.Error
import Network.Salvia.Handler.Parser
import Prelude hiding ((.), id)
import System.IO
import System.Process
import qualified Data.ByteString as B

-- | Handler to run CGI scripts. Not entirely finished.

hCGI :: (MonadIO m, BodyM Request m, HttpM' m, SendM m, QueueM m, ServerM m) => FilePath -> m ()
hCGI fn =
  do cfg     <- server
     hdrs    <- request (getM headers)
     _query  <- request (getM (query . asUri))
     _path   <- request (getM (path . asUri))
     _method <- request (getM method)

     -- Helper function to convert all headers to environment variables.
     let headerDecls =
           map (\(a, b) -> ("HTTP_" ++ (map toUpper . intercalate "_" . splitOn "-") a, b))
             . toList . unHeaders

     -- Set the of expoerted server knowledge.
     let envs =
             ("GATEWAY_INTERFACE", "CGI/1.1")
           : ("REQUEST_METHOD",    show _method)
           : ("REQUEST_URI",       _path)
           : ("QUERY_STRING",      _query)
           : ("SERVER_SOFTWARE",   "Salvia")
           : ("SERVER_SIGNATURE",  "")
           : ("SERVER_PROTOCOL",   "HTTP/1.1")
           : ("SERVER_ADDR",       show (listenAddr cfg)) -- todo: fix show.
           : ("SERVER_ADMIN",      admin cfg)
           : ("SERVER_NAME",       hostname cfg)
           : ("SERVER_PORT",       show (listenPort cfg))
           : ("REMOTE_ADDR",       "") -- todo
           : ("REMOTE_PORT",       "") -- todo
           : ("SCRIPT_FILENAME",   "") -- todo
           : ("SCRIPT_NAME",       "") -- todo
           : headerDecls hdrs

     -- Start up the CGI script with the appropriate environment variables.
     -- todo: what to do with stderr? log?
     (inp, out, _, pid) <- liftIO (runInteractiveProcess fn [] Nothing $ Just envs)

     -- Read the request body and fork a thread to spool the body to the CGI
     -- script's input. After spooling, or when there is no data, the scripts
     -- input will be closed.
     b <- body forRequest
     liftIO $
       case b of
         Nothing -> hClose inp
         Just b' -> forkIO (B.hPut inp b' >> hClose inp) >> return ()

     -- Read the headers produced by the CGI script and store them as the
     -- response headers of this handler.
     hs <- liftIO (readNonEmptyLines out)
     case parseHeaders hs of
       Left e  -> hCustomError InternalServerError e
       Right r -> response (headers =: r)

     -- Spool all data from the CGI script's output to the client. When
     -- finished, close the handle and wait for the script to terminate.
     spool out
     enqueue (const (hClose out <* waitForProcess pid))

