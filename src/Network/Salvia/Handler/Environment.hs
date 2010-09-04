{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.Environment
( hDefaultEnv
, hEnvNoKeepAlive
)
where

import Control.Monad.CatchIO (MonadCatchIO)
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
  :: (MonadCatchIO m, HandleM m, RawHttpM' m, HttpM' m, QueueM m, SendM m, FlushM Response m)
  => m ()  -- ^ Handler to run in the default environment.
  -> m ()
hDefaultEnv handler =
  hKeepAlive . hCatchAnd500 $
    do hBanner "salvia-httpd"
       _ <- hRequestParser (1000 * 4)
         (hCustomError BadRequest)
         (hHead handler)
       hResponsePrinter

-- | Like `hDefaultEnv' but only serves one request per connection.

hEnvNoKeepAlive
  :: (MonadCatchIO m, HandleM m, RawHttpM' m, HttpM' m, QueueM m, SendM m, FlushM Response m)
  => m ()  -- ^ Handler to run in this environment.
  -> m ()
hEnvNoKeepAlive handler =
  hCatchAnd500 $
    do hBanner "salvia-httpd"
       _ <- hRequestParser (1000 * 4)
         (hCustomError BadRequest)
         (hHead handler)
       hResponsePrinter

