module Main where

import Control.Monad.Trans
import Network.Socket
import Network.Salvia.Httpd
import Network.Salvia.Handlers

main :: IO ()
main =
  do addr <- inet_addr "127.0.0.1"
     putStrLn "started"
     start 
       (defaultConfig { listenAddr = addr, listenPort = 8080 })
       (hDefaultEnv myHandler)
       ()

-- Serve the current directory.

myHandler :: (MonadIO m, Request m, Response m, Send m) => m ()
myHandler = hFileSystem "."

