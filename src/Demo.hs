module Main where

import Control.Monad.Trans
import Network.Socket
import Network.Protocol.Http hiding (server)
import Network.Salvia

main :: IO ()
main =
  do addr <- inet_addr "127.0.0.1"
     putStrLn "started"
     server 
       (defaultConfig { listenAddr = addr, listenPort = 8080 })
       (hDefaultEnv myHandler)
       ()

-- Serve the current directory.

myHandler :: (MonadIO m, HttpM Request m, HttpM Response m, SendM m) => m ()
myHandler = hFileSystem "."

-- test :: IO (Maybe String)
-- test = getRequest "http://www.google.nl/search?q=aap"


