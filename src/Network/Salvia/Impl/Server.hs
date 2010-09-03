module Network.Salvia.Impl.Server (start) where

import Control.Concurrent (myThreadId)
import Control.Concurrent.Thread (ThreadId, forkIO, wait_, threadId)
import Control.Concurrent.MVar (newMVar, modifyMVar_, readMVar)
import Control.Exception (finally, block, unblock)
import Control.Monad.State
import Network.Protocol.Http hiding (accept, hostname)
import Network.Salvia.Impl.Config
import Network.Salvia.Impl.Context
import Network.Salvia.Impl.Handler
import Network.Socket
import System.IO

{- |
Start a webserver with a specific server configuration and default handler. The
server will go into an infinite loop and will repeatedly accept client
connections on the address/port combinations specified in the configuration.
For every connection the handler will be executed with the client address and
socket stored in the handler context.
-}

start :: Config -> Handler p () -> p -> IO ()
start conf handler payload =
  do listeners <- forM (listenOn conf) (forkIO . ipv4only startOne)
     waitAll listeners

  where

  startOne port addr =
    do -- Print out on which addres this server is running.
       a <- inet_ntoa addr
       putStrLn ("Starting listening server on: " ++ a ++ ":" ++ show port)

       -- Setup socket and start listening.
       s <- socket AF_INET Stream 0
       setSocketOption s ReuseAddr 1
       let sAddr = SockAddrInet port addr
       bindSocket s sAddr
       listen s (backlog conf)

       -- Setup variable to store thread IDs.
       tids <- newMVar []

       -- Go into loop to accept incoming connections.
       let waitForAll = readMVar tids >>= waitAll
       forever (acceptHandle sAddr tids s)
         `finally` waitForAll

       where

         acceptHandle sAddr tids s = do
           a <- accept s
           modifyMVar_ tids $ \handlersTids ->
               fmap (:handlersTids) $ block $ forkIO $
                    unblock (handle sAddr a) `finally` deleteMyTid tids

         handle sAddr (sck, cAddr) = do
           hndl <- socketToHandle sck ReadWriteMode
           _ <- runHandler handler Context
                { _cServerHost  = domain    conf
                , _cAdminMail   = adminMail conf
                , _cListenOn    = listenOn  conf
                , _cPayload     = payload
                , _cRequest     = emptyRequest
                , _cResponse    = emptyResponse
                , _cRawRequest  = emptyRequest
                , _cRawResponse = emptyResponse
                , _cSocket      = sck
                , _cHandleIn    = hndl
                , _cHandleOut   = hndl
                , _cClientAddr  = cAddr
                , _cServerAddr  = sAddr
                , _cQueue       = []
                }
           return ()

         deleteMyTid tids = do
           myTid <- myThreadId
           modifyMVar_ tids $ \handlersTids ->
               return $! deleteWhen' ((myTid ==) . threadId) handlersTids

ipv4only :: (PortNumber -> HostAddress -> IO a) -> SockAddr -> IO a
ipv4only _ (SockAddrInet6 _ _ _ _ ) = error (funcName ++ ": IPv6 sockets are not yet supported!")
ipv4only _ (SockAddrUnix _        ) = error (funcName ++ ": Unix sockets are not yet supported!")
ipv4only c (SockAddrInet port addr) = c port addr

funcName :: String
funcName = "Network.Salvia.Impl.Server.start"

-- | Block until all threads identified by the given @ThreadId@s are terminated.
waitAll :: [ThreadId ()] -> IO ()
waitAll = mapM_ wait_

{-| Strictly delete the first element of the list for which the predicate holds.

Note that this function has the following strictness properties:

@deleteWhen' (== 2) (1:2:3:undefined) = 1:3:undefined@
@deleteWhen' (== 3) (1:2:3:undefined) = undefined@
-}
deleteWhen' :: (a -> Bool) -> [a] -> [a]
deleteWhen' p = deleteWhen'_p
    where
      deleteWhen'_p []     = []
      deleteWhen'_p (x:xs)
          | p x            = xs
          | otherwise      = (x:) $! deleteWhen'_p xs

