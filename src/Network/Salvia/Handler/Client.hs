module Network.Salvia.Handler.Client where

import Control.Monad.State hiding (get)
import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Core.Aspects
import Network.Salvia.Handler.Parser
import Network.Salvia.Handler.Printer
import qualified Control.Monad.State as S

hGetRequest :: (HttpM Request m, FlushM Request m) => String -> m ()
hGetRequest s =
  do let u = toUri s
     request $
       do method     =: GET
          uri        =: (get path u ++ "?" ++ get query u)
          hostname   =: Just (show (get authority u))
          userAgent  =: Just "salvia-client"
          connection =: Just "close"
     hRequestPrinter
     return ()

hClientEnvironment
  :: (SocketM m, HttpM Response m, MonadIO m)
  => (String -> m a) -> m a -> m (Maybe a)
hClientEnvironment = hResponseParser (4 * 1000)

