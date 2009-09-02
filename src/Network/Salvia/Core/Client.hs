module Network.Salvia.Core.Client where

import Control.Applicative
import Control.Monad.State hiding (get)
import Data.Maybe
import Data.Record.Label
import Network.BSD
import Network.Protocol.Uri
import Network.Salvia.Core.Aspects
import Network.Salvia.Core.Handler
import Network.Salvia.Handler.Body
import Network.Salvia.Handler.Client
import Network.Socket
import System.IO
import qualified Network.Salvia.Core.Context as C

client
  :: Handler c () a -- ^ Handler to setup request.
  -> Handler d () b -- ^ Handler to react to response.
  -> String         -- ^ jaja
  -> IO (Maybe b)  -- ^ jaja

client hReq hRes uri = 
  case parseUri uri of
    Left _ -> return Nothing
    Right u ->
      do let p = fromMaybe 80 (get port u)

         -- Open up connection to client
         entry <- getHostByName (get host u)
         sck <- socket (hostFamily entry) Stream 0
         let addr = head (hostAddresses entry)
         connect sck (SockAddrInet (fromIntegral p) addr)
         h <- socketToHandle sck ReadWriteMode
         name <- getSocketName sck

         -- Create context and run setup request.
         ss <- execStateT (unHandler hReq) (C.mkContext undefined () name sck h)

         -- Update context and handle response.
         let k = request (put (get C.request ss)) >> hRes
         Just <$> evalStateT (unHandler k) (C.mkContext undefined () name sck h)

getRequest :: String -> IO (Maybe String)
getRequest u = join . join <$>
  client 
    (hGetRequest u)
    (hClientEnvironment
      (const (return Nothing))
      (hResponseBody "utf-8")
    ) u

