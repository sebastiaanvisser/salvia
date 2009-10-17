module Main where

import Control.Monad.Trans
import Data.Record.Label
import Network.Protocol.Http hiding (server, hostname)
import Network.Salvia hiding (server)
import Network.Salvia.Impl.Server
import Network.Socket
import System.IO

main :: IO ()
main =
  do addr <- inet_addr "127.0.0.1"
     putStrLn "started"
     store <- mkSessions :: IO (Sessions Int)
     l <- return stdout -- openFile "salvia-demo.log" AppendMode
     server 
       (defaultConfig { listenAddr = addr, listenPort = 8080, hostname = "127.0.0.1" })
       (hDefaultEnv l myHandler)
       store

myHandler :: (HttpM Response m, MonadIO m, SessionM m Int, HttpM Request m, QueueM m, BodyM Request m, ServerM m) => m ()
myHandler = 
  hPortRouter
    [ ( 8080
      , hVirtualHosting
          [ ("127.0.0.1",
              hPrefixRouter [ ("/session", hPrintSession) ]
              (hRedirect "http://localhost:8080/")
            )
          , ("localhost", 
              hPrefixRouter [ ("/put-bin", hPutFileSystem "www/data") ]
              (hFileSystem ".")
            )
--           , (".host", hProxy "www.google.com")
          , ("phony", 
               hPrefix "/favicon.ico"
                 (hError NotFound)
                 (hCGI "./myscript.sh")
            )
          ] (hError Forbidden)
      )
    ] (hCustomError Forbidden "Public service running on port 8080.")

hPrintSession :: (HttpM Request m, SessionM m Int, MonadIO m) => m ()
hPrintSession =
--   do r <- request S.get
--      liftIO (print r)
  do s <- useSession (60 * 1)
--      putSession (s { sPayload = Just (20 :: Int)} )
     liftIO (print (s :: Session Int))

hString :: (HttpM Response m, QueueM m) => String -> m ()
hString s =
  do response $
       do contentType   =: Just ("text/plain", Just "utf-8")
          contentLength =: Just (length s)
     hSend s

