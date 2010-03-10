{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.Proxy where

import Control.Applicative
import Control.Category
import Control.Monad.State
import Data.Maybe
import Data.ByteString.Lazy
import Data.Record.Label hiding (get)
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Interface
import Network.Salvia.Impl.Client
import Network.Salvia.Handler.Client
import Network.Salvia.Handler.Parser
import Network.Salvia.Handler.Printer
import Prelude hiding ((.), id)
import qualified Control.Monad.State as S
import qualified Data.Record.Label as L

hProxy :: (HttpM' m, SendM m, MonadIO m) => String -> m ()
hProxy host =
  do req <- request get
     liftIO (print ("hProxy", L.get uri req))
     liftIO (print ("hProxy", L.get hostname req))
     r <- liftIO $ runClient (hReq req) hRes
     case r of
       Just (Right (res, bdy)) ->
         do response (put res)
            sendBs bdy
       a -> liftIO (print a)

hReq :: HttpM Request m => Http Request -> m ()
hReq req =
  do request (put req)
     let mapper = fromJust . remap (toUri "http://host:8080", toUri "http://google.com")
     request (modM asUri mapper)

hRes =
  do hRequestPrinter
     hResponseParser (4 * 1000)
       (\e -> return (Left e))
       (Right <$> ((,) <$> response get <*> body forResponse))

