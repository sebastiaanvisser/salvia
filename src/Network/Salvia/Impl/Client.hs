module Network.Salvia.Impl.Client where

import Control.Applicative
import Control.Monad.State hiding (get)
import Data.Maybe
import Data.Record.Label
import Network.BSD
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Core.Aspects
import Network.Salvia.Core.Context
import Network.Salvia.Impl.Handler
import Network.Socket
import System.IO
import System.IO.Error

runClient
  :: ClientHandler a -- ^ Handler to setup request.
  -> ClientHandler b -- ^ Handler to react to response.
  -> IO b

runClient hReq hRes = 
  do -- Run the handler to setup the request.
     req <- get cRequest . snd <$> runHandler hReq emptyContext
     let u = get asUri req
         p = fromMaybe 80 (get port u)
         h = get host u

     -- Get the host address first by trying a hostname lookup and when this
     -- fails trying to parse as an IP address.
     hbn <- try (getHostByName h)
     (fam, addr) <- case hbn of
       Left _  -> inet_addr h >>= return . (,) AF_INET
       Right e -> return (hostFamily e, head (hostAddresses e))

     -- Open up connection to client
     sck <- socket fam Stream 0
     connect sck (SockAddrInet (fromIntegral p) addr)
     fd <- socketToHandle sck ReadWriteMode
     name <- getSocketName sck

     -- Put the request in the context and run the response handler.
     fst <$> runHandler 
       (request (put req) >> hRes)
       (mkContext () () name (error "help help aap help") sck fd)

-- getRequest :: String -> IO (Maybe String)
-- getRequest u = join . join <$>
--   runClient 
--     (hGetRequest u)
--     (hClientEnvironment
--       (const (return Nothing))
--       (hResponseBody "utf-8")
--     ) u

