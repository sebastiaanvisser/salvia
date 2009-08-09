module Network.Salvia.Core.Client where

import Control.Monad.State
import Data.Record.Label
import Network.Protocol.Uri
import Network.Socket
import Network.Salvia.Httpd hiding (hostname)
import qualified Network.Salvia.Core.Context as C
import Network.Salvia.Core.Handler
import System.IO
import Network.BSD

hClient :: Handler a -> Handler b -> String -> IO Bool
hClient hReq hRes uri = 
  do let u = toURI uri
     entry <- getHostByName (lget host u)
     sck <- socket (hostFamily entry) Stream 0
     let p = let p' = lget port u in if p' == (-1) then 80 else p'
     let addr = head (hostAddresses entry)
     connect sck (SockAddrInet (fromIntegral p) addr)
     h <- socketToHandle sck ReadWriteMode
     name <- getSocketName sck

     let ctx = C.mkContext (error "no server config") name sck h
     ss <- execStateT (unHandler hReq) ctx

     let ctx' = C.mkContext (error "no server config") name sck h
     let k = request (put (lget C.request ss)) >> hRes
     evalStateT (unHandler k) ctx'
     return True

-- test :: IO (Either () Message)
-- test = client $ either undefined id $ parseURI "http://17.149.160.31/" -- "http://www.google.nl/search?q=aap"

