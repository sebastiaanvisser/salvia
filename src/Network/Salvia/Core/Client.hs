module Network.Salvia.Core.Client where

import Control.Monad.State
import Data.Record.Label
import Network.Protocol.Uri
import Network.Socket
import qualified Network.Salvia.Core.Context as C
import Network.Salvia.Core.Handler
import Network.Salvia.Core.Aspects
import Network.Salvia.Handler.Client
import Network.Salvia.Handler.Contents
import System.IO
import Network.BSD

client :: Handler c () a -> Handler c1 () b -> String -> IO b
client hReq hRes uri = 
  do let u = toURI uri
     entry <- getHostByName (lget host u)
     sck <- socket (hostFamily entry) Stream 0
     let p = let p' = lget port u in if p' == (-1) then 80 else p'
     let addr = head (hostAddresses entry)
     connect sck (SockAddrInet (fromIntegral p) addr)
     h <- socketToHandle sck ReadWriteMode
     name <- getSocketName sck

     let ctx = C.mkContext (error "no server config") () name sck h
     ss <- execStateT (unHandler hReq) ctx

     let ctx' = C.mkContext (error "no server config") () name sck h
     let k = request (put (lget C.request ss)) >> hRes
     evalStateT (unHandler k) ctx'




getRequest :: String -> IO (Maybe String)
getRequest u = join `liftM`
  client 
    (hGetRequest u)
    (hClientEnvironment
      (const (return Nothing))
      (asASCII hResponseContents)
    ) u

