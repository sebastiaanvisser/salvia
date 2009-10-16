module Network.Salvia.Handler.Environment {- doc ok -}
  ( hDefaultEnv
--   , hSessionEnv
  )
where

import Control.Monad.State
import Control.Concurrent.STM
import Network.Protocol.Http
import Network.Salvia.Handler.Banner
import Network.Salvia.Handler.Counter
import Network.Salvia.Handler.Close
import Network.Salvia.Handler.Error
import Network.Salvia.Handler.Head
import Network.Salvia.Handler.Log
import Network.Salvia.Handler.Parser
import Network.Salvia.Handler.Printer
-- import Network.Salvia.Handler.Session
import Network.Salvia.Core.Aspects
import System.IO
import Prelude hiding (log)

{- |
This is the default stateless handler evnironment. It takes care of request
parsing (`hRequestParser`), response printing (`hResponsePrinter`), request
logging (`hLog`), connection keep-alives (`hKeepAlive`), handling `HEAD`
requests (`hHead`) and printing the `salvia-httpd` server banner (`hBanner`).
-}

hDefaultEnv
  :: (MonadIO m, FlushM Response m, PeerM m, HttpM Request m, HttpM Response m, ServerM m, QueueM m)
  => Handle  -- ^ File handle to log to.
  -> m a     -- ^ Handler to run in the default environment.
  -> m ()
hDefaultEnv log handler = wrapper log Nothing (hHead handler)

{- |
This function is a more advanced version of the `hDefaultEnv` handler
environment that takes a global state into account. It takes a shared variable
containg the connection counter (used by `hCounter`) and a variable containing
all session information (used by `hSession`). Handlers that run in this
environment should be parametrized with a session.
-}

{-hSessionEnv
  :: (MonadIO m, FlushM Response m, QueueM m, PeerM m, HttpM Request m, HttpM Response m, ServerM m)
  => Handle                 -- ^ File handle to log to.
  -> TVar Int               -- ^ Request count variable.
  -> Sessions b             -- ^ Session collection variable.
  -> (TSession b -> m a)    -- ^ m parametrized with current session.
  -> m ()
hSessionEnv log count sessions handler =
  wrapper log (Just count) $
    do session <- hSession sessions 300
       hHead (handler session)-}

-- Helper with common functionality.

wrapper
  :: (MonadIO m, HttpM Response m, HttpM Request m, FlushM Response m, PeerM m, QueueM m)
  => Handle -> Maybe (TVar Int) -> m a -> m ()
wrapper log count handler = 
  let logger = maybe (hLog log) (\c -> hCounter c >> hLogWithCounter c log) count
      f h = h >> hResponsePrinter >> logger
  in hKeepAlive $ 
    do hBanner "salvia-httpd"
       hRequestParser (1000 * 4)
         (f . hCustomError BadRequest)
         (f handler)

