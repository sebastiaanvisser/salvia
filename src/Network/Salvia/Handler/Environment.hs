{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.Environment (hDefaultEnv) where

import Control.Monad.State
import Network.Protocol.Http
import Network.Salvia.Interface
import Network.Salvia.Handler.Banner
import Network.Salvia.Handler.Close
import Network.Salvia.Handler.Error
import Network.Salvia.Handler.Head
import Network.Salvia.Handler.Parser
import Network.Salvia.Handler.Printer
import Prelude hiding (log)

{- |
This is the default handler environment. It takes care of request parsing
(`hRequestParser`), response printing (`hResponsePrinter`), connection
keep-alives (`hKeepAlive`), handling `HEAD` requests (`hHead`) and printing the
`salvia-httpd` server banner (`hBanner`). 
-}

hDefaultEnv
  :: (MonadIO m, SockM m, RawHttpM' m, HttpM' m, QueueM m, SendM m, FlushM Response m)
  => m ()  -- ^ Handler to run in the default environment.
  -> m ()
hDefaultEnv handler =
  hKeepAlive $ 
    do hBanner "salvia-httpd"
       _ <- hRequestParser (1000 * 4)
         (hCustomError BadRequest)
         (hHead handler)
       hResponsePrinter

