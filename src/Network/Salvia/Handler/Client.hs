module Network.Salvia.Handler.Client where

import Control.Monad.State
import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Socket
import Network.Salvia.Httpd hiding (hostname)
import Network.Salvia.Handler.Parser
import Network.Salvia.Handler.Printer
import Network.Salvia.Handler.Contents
import qualified Network.Salvia.Core.Context as C
import Network.Salvia.Core.Handler
import System.IO
import Network.BSD
import qualified Data.Map as M

-- test :: IO (Either () Message)
-- test = client $ either undefined id $ parseURI "http://17.149.160.31/" -- "http://www.google.nl/search?q=aap"

hGetRequest s =
  do let ur = parseURI s
     case ur of
       Left e -> return ()
       Right u ->
         do request $
              do setM method GET
                 setM uri (lget path u ++ "?" ++ lget query u)
                 setM hostname (lget authority u)
                 setM userAgent "salvia-client"
            hRequestPrinter
            return ()

hClientEnvironment onfail onsucc =
  do liftIO (putStrLn "asdasdasd")
     hResponseParser (4 * 1000) onfail onsucc


myHandler =
  do q <- request get
     liftIO (print q)
     r <- response get
     liftIO (print r)
     c <- asUTF8 hResponseContents
     liftIO (putStrLn ((\(Just s) -> s) c))
     return ()

{-test =
  let uri = "http://www.google.nl/search?q=aap"
  in hClient
       (hGetRequest uri)
       (hClientEnvironment undefined myHandler)
       uri-}

