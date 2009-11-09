module Network.Salvia.Impl.Server (start) where

import Control.Concurrent.ThreadManager
import Control.Monad.State
import Network.Salvia.Core.Config
import Network.Salvia.Core.Context 
import Network.Salvia.Impl.Handler
import Network.Socket
import System.IO

{- | todo:
Start a webserver with a specific server configuration and default handler. The
server will go into an infinite loop and will repeatedly accept client
connections on the address and port specified in the configuration. For every
connection the specified handler will be executed with the client address and
socket stored in the handler context.
-}

{- todo:
Start a listening TCP server on the specified address/port combination and
handle every connection with a custom handler. Accept connections on the
listening socket and pass execution to the application specific connection
handler.
-}

start :: Config -> Handler Config p () -> p -> IO ()
start conf handler payload =
  do tm <- make
     forM_ (listenOn conf) $ \(SockAddrInet port addr) ->
       fork tm $
         do inet_ntoa addr >>= \a ->
              putStrLn ("starting listening server on: " ++ a ++ ":" ++ show port)
            s <- socket AF_INET Stream 0
            setSocketOption s ReuseAddr 1
            let sAddr = SockAddrInet port addr
            bindSocket s sAddr
            listen s (backlog conf)
            forever $
              do (raw, cAddr) <- accept s
                 fork tm $
                   do sck <- socketToHandle raw ReadWriteMode
                      evalStateT (unHandler handler) (mkContext conf payload cAddr sAddr raw sck)
     waitForAll tm

