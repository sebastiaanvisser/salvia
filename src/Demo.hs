module Main where

import Control.Monad.Trans
import Network.Socket
import Network.Salvia.Httpd
import Network.Salvia.Handlers

-- Serve the current directory.

main :: IO ()
main =
  do conf <- defaultConfig
     addr <- inet_addr "127.0.0.1"
     putStrLn "started"
     start 
       (conf { listenAddr = addr, listenPort = 8080 })
       (hDefaultEnv myHandler)

-- Serve the current directory.

myHandler :: (MonadIO m, Request m, Response m, Send m) => m ()
myHandler = hFileSystem "."

