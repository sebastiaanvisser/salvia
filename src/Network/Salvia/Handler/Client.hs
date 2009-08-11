module Network.Salvia.Handler.Client where

import Control.Monad.State
import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Core.Aspects
import Network.Salvia.Handler.Contents
import Network.Salvia.Handler.Parser
import Network.Salvia.Handler.Printer
import System.IO

hGetRequest :: (Request m, Send m) => String -> m ()
hGetRequest s =
  do let u = toURI s
     request $
       do setM method     GET
          setM uri        (lget path u ++ "?" ++ lget query u)
          setM hostname   (lget authority u)
          setM userAgent  "salvia-client"
          setM connection "close"
     hRequestPrinter
     return ()

hClientEnvironment
  :: (Socket m, Response m, MonadIO m) =>
     (String -> m a) -> m a -> m (Maybe a)
hClientEnvironment = hResponseParser (4 * 1000)


cHandler :: (Request m, Socket m, MonadIO m, Response m) => m ()
cHandler =
  do q <- request get
     liftIO (print q)
     r <- response get
     liftIO (print r)
     c <- asASCII hResponseContents
     liftIO (putStr ((\(Just s) -> s) c))
     return ()




