module Network.Salvia.Impl.Server (server) where

import Control.Concurrent
import Control.Monad.State
import Network.Salvia.Core.Context 
import Network.Salvia.Impl.Handler
import Network.Salvia.Core.Config
import Network.Socket
import System.IO

{- |
Start a webserver with a specific server configuration and default handler. The
server will go into an infinite loop and will repeatedly accept client
connections on the address and port specified in the configuration. For every
connection the specified handler will be executed with the client address and
socket stored in the handler context.
-}

{-
Start a listening TCP server on the specified address/port combination and
handle every connection with a custom handler. Accept connections on the
listening socket and pass execution to the application specific connection
handler.
-}

server :: Config -> Handler Config p () -> p -> IO ()
server c h p = do
  s <- socket AF_INET Stream 0
  setSocketOption s ReuseAddr 1
  bindSocket s $ SockAddrInet (listenPort c) (listenAddr c)
  listen s (backlog c)
  forever $
    do (k, a) <- accept s
       forkIO $
         do d <- socketToHandle k ReadWriteMode
            let ctx = mkContext c p a k d
            evalStateT (unHandler h) ctx

