module Network.Salvia.Handler.Environment (
    hDefaultEnv
  , hSessionEnv
  ) where

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
import Network.Salvia.Handler.Session
import Network.Salvia.Httpd
import System.IO

{- |
This is the default stateless handler evnironment. It takes care of request
parsing (`hParser`), response printing (`hPrinter`), request logging (`hLog`),
connection keep-alives (`hKeepAlive`), handling `HEAD` requests (`hHead`) and
printing the `salvia-httpd` server banner (`hBanner`).
-}

hDefaultEnv
  :: (MonadIO m, Socket m, Request m, Response m, Config m, Send m)
  => m a     -- ^ Handler to run in the default environment.
  -> m ()
hDefaultEnv handler =
  hKeepAlive $ 
    hParser (1000 * 15)
      (wrapper Nothing . parseError)
      (wrapper Nothing $ hHead handler)

{- |
This function is a more advanced version of the `hDefaultEnv` handler
environment that takes a global state into account. It takes a shared variable
containg the connection counter (used by `hCounter`) and a variable containing
all session information (used by `hSession`). Handlers that run in this
environment take should be parametrized with a session.
-}

hSessionEnv
  :: (MonadIO m, Socket m, Request m, Response m, Config m, Send m)
  => TVar Int               -- ^ Request count variable.
  -> Sessions b             -- ^ Session collection variable.
  -> (TSession b -> m a)    -- ^ m parametrized with current session.
  -> m ()
hSessionEnv count sessions handler =
  hKeepAlive $ 
    hParser (1000 * 15)
     (wrapper (Just count) . parseError)
     (wrapper (Just count) $
       do session <- hSession sessions 300
          hHead (handler session))

-- Helper functions.


before :: (MonadIO m, Response m) => m ()
before = hBanner "salvia-httpd"

after
  :: (Socket m, Request m, Config m, MonadIO m, Response m)
  => Maybe (TVar Int) -> m ()
after mc = 
  do hPrinter
     maybe
       (hLog stdout)
       (\c -> hCounter c >> hLogWithCounter c stdout)
       mc

wrapper
  :: (MonadIO m, Response m, Config m, Socket m, Request m)
  => Maybe (TVar Int) -> m a -> m ()
wrapper c h = before >> h >> after c

parseError :: (Response m, Send m) => String -> m ()
parseError err = 
  do hError BadRequest
     sendStrLn []
     sendStrLn err

