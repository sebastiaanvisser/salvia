module Network.Salvia.Impl.Server (start) where

import Control.Concurrent (myThreadId)
import Control.Concurrent.Thread (ThreadId, forkIO, wait_, threadId)
import Control.Concurrent.MVar (newMVar, modifyMVar_, readMVar)
import Control.Exception (finally, block, unblock)
import Control.Monad.State
import Data.Functor ((<$))
import Network.Protocol.Http hiding (accept, hostname)
import Network.Salvia.Impl.Config
import Network.Salvia.Impl.Context
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

start :: Config -> Handler p () -> p -> IO ()
start conf handler payload = forM (listenOn conf) (forkIO . listener) >>= waitAll
    where
      listener (SockAddrInet6 _ _ _ _) = error $ funcName ++ ": IPv6 sockets are not supported!"
      listener (SockAddrUnix _)        = error $ funcName ++ ": Unix sockets are not supported!"
      listener (SockAddrInet port addr) = do
        inet_ntoa addr >>= \a ->
          putStrLn ("starting listening server on: " ++ a ++ ":" ++ show port)
        s <- socket AF_INET Stream 0
        setSocketOption s ReuseAddr 1
        bindSocket s sAddr
        listen s (backlog conf)
        handlersTidsMVar <- newMVar []
        forever (acceptHandle handlersTidsMVar s) `finally` (readMVar handlersTidsMVar >>= waitAll)
        where
          sAddr = SockAddrInet port addr

          acceptHandle handlersTidsMVar s = do
            a <- accept s
            modifyMVar_ handlersTidsMVar $ \handlersTids ->
                fmap (:handlersTids) $ block $ forkIO $
                     unblock (handle a) `finally` deleteMyTid handlersTidsMVar

          handle (sck, cAddr) = do
            hndl <- socketToHandle sck ReadWriteMode
            void $ runHandler handler Context
                 { _cServerHost  = hostname conf
                 , _cAdminMail   = adminMail conf
                 , _cListenOn    = listenOn conf
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

          deleteMyTid handlersTidsMVar = do
            myTid <- myThreadId
            modifyMVar_ handlersTidsMVar $ \handlersTids ->
                return $! deleteWhen' ((myTid ==) . threadId) handlersTids

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

void :: Functor f => f a -> f ()
void = (() <$)
