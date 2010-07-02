{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.CGI (hCGI) where

import Control.Applicative
import Control.Category
import Control.Concurrent
import Control.Monad.State
import Data.Char
import Data.List
import Data.List.Split
import Data.Record.Label
import Network.Protocol.Http hiding (server)
import Network.Protocol.Uri hiding (host)
import Network.Salvia.Interface
import Network.Salvia.Handler.Error
import Network.Salvia.Handler.Parser
import Network.Socket
import Prelude hiding ((.), id)
import System.IO
import System.Process
import qualified Data.ByteString.Lazy as B

-- | Handler to run CGI scripts.

-- todo: fails on ipv6 en unix sockets.
-- todo: stderr?

hCGI :: (MonadIO m, HttpM' m, BodyM Request m, SendM m, HandleQueueM m, ServerM m, AddressM' m) => FilePath -> m ()
hCGI fn =
  do adm <- serverAdmin
     hst <- serverHost
     (cp, ca) <- clientAddress >>= addr
     (sp, sa) <- serverAddress >>= addr
     hdrs    <- request (getM headers)
     _query  <- request (getM (query . asUri))
     _path   <- request (getM (path . asUri))
     _method <- request (getM method)

     -- Helper function to convert all headers to environment variables.
     let headerDecls =
           map (\(a, b) -> ("HTTP_" ++ (map toUpper . intercalate "_" . splitOn "-") a, b))
             . unHeaders

     -- Set the of expoerted server knowledge.
     let envs =
             ("GATEWAY_INTERFACE", "CGI/1.1")
           : ("REQUEST_METHOD",    show _method)
           : ("REQUEST_URI",       _path)
           : ("QUERY_STRING",      _query)
           : ("SERVER_SOFTWARE",   "Salvia")
           : ("SERVER_SIGNATURE",  "")
           : ("SERVER_PROTOCOL",   "HTTP/1.1")
           : ("SERVER_ADMIN",      adm)
           : ("SERVER_NAME",       hst)
           : ("SERVER_ADDR",       sa)
           : ("SERVER_PORT",       show sp)
           : ("REMOTE_ADDR",       ca)
           : ("REMOTE_PORT",       show cp)
           : ("SCRIPT_FILENAME",   fn)
           : ("SCRIPT_NAME",       fn)
           : headerDecls hdrs

     -- Start up the CGI script with the appropriate environment variables.
     -- todo: what to do with stderr? log?
     (inp, out, _, pid) <- liftIO (runInteractiveProcess fn [] Nothing $ Just envs)

     -- Read the request body and fork a thread to spool the body to the CGI
     -- script's input. After spooling, or when there is no data, the scripts
     -- input will be closed.
     b <- body forRequest
     liftIO $ forkIO (B.hPut inp b >> hClose inp) >> return ()

     -- Read the headers produced by the CGI script and store them as the
     -- response headers of this handler.
     hs <- liftIO (readNonEmptyLines out)
     case parseHeaders hs of
       Left e  -> hCustomError InternalServerError e
       Right r -> response (headers =: r)

     -- Spool all data from the CGI script's output to the client. When
     -- finished, close the handle and wait for the script to terminate.
     spool out
     enqueueHandle (const (hClose out <* waitForProcess pid))

  where
  addr (SockAddrInet  p a)     = (,) p <$> liftIO (inet_ntoa a)
  addr (SockAddrInet6 p _ _ _) = return (p,  "ipv6")
  addr _                       = return (-1, "unix")


