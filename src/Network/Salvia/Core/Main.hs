module Network.Salvia.Core.Main (start) where

import Control.Concurrent (forkIO)
import Control.Monad.State
import Network.Salvia.Core.Config (listenAddr, listenPort, backlog, HttpdConfig)
import Network.Salvia.Core.Context (mkContext)
import Network.Salvia.Core.Handler
import Network.Socket
import System.IO

{- |
Start a webserver with a specific server configuration and default handler. The
server will go into an infinite loop and will repeatedly accept client
connections on the address and port specified in the configuration. For every
connection the specified handler will be executed with the client address and
socket stored in the handler context.
-}

start
  :: HttpdConfig
  -> Handler ()
  -> IO ()
start config httpHandler =
  server
    (listenAddr config)
    (listenPort config)
    (backlog config)
    tcpHandler
  where
    tcpHandler addr raw handle =
      do let ctx = mkContext config addr raw handle
         evalStateT (unHandler httpHandler) ctx

{-
Start a listening TCP server on the specified address/port combination and
handle every connection with a custom handler. Accept connections on the
listening socket and pass execution to the application specific connection
handler.
-}

server
  :: HostAddress
  -> PortNumber
  -> Int
  -> (SockAddr -> Socket -> Handle -> IO ())
  -> IO ()
server addr port blog handler = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock $ SockAddrInet port addr
  listen sock blog
  forever $
    do (c, a) <- accept sock
       forkIO (socketToHandle c ReadWriteMode >>= handler a c)
  putStrLn "quiting"

