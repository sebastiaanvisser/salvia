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
myHandler = 
  hPortRouter
    [ ( 8080
      , hVirtualHosting
          [ ("127.0.0.1", hRedirect "http://localhost:8080/")
          , ("localhost", hFileSystem ".")
          , (".host",     hFileSystem "/Users/sebas/Pictures")
          , ("phony",     hCustomError NotFound "phony!")
          ] (hError Forbidden)
      )
    ] (hCustomError Forbidden "Public service running on port 8080.")





-- test :: IO (Maybe String)
-- test = getRequest "http://www.google.nl/search?q=aap"


