module Main where

import Control.Monad.Trans
import Data.Record.Label
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

myHandler :: (HttpM Response m, MonadIO m, HttpM Request m, SendM m, BodyM Request m, ConfigM m) => m ()
myHandler = 
  hPortRouter
    [ ( 8080
      , hVirtualHosting
          [ ("127.0.0.1", hRedirect "http://localhost:8080/")
          , ("localhost",
              hPrefixRouter [ ("/put", hPutFileSystem "www/data") ]
              (hFileSystem ".")
            )
          , (".host", hString "Jajaj!!!")
          , ("phony", 
               hPrefix "/favicon.ico"
                 (hError NotFound)
                 (hCGI "./myscript.sh")
            )
          ] (hError Forbidden)
      )
    ] (hCustomError Forbidden "Public service running on port 8080.")

hString :: (HttpM Response m, SendM m) => String -> m ()
hString s =
  do response $
       do contentType   =: Just ("text/plain", Just "utf-8")
          contentLength =: Just (length s)
     sendStr s

-- test :: IO (Maybe String)
-- test = getRequest "http://www.google.nl/search?q=aap"

