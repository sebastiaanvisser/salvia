module Network.Salvia.Handler.CGI (hCGI) where {- doc ok -}

import Control.Monad.State
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Core.Aspects
import System.IO
import System.Process

-- | Handle CGI scripts, not yet working properly.

hCGI :: (HttpM Response m, MonadIO m, SocketM m) => FilePath -> m ()
hCGI fn =
  do response (status =: OK)
     h <- sock
     liftIO $
       do p <- runProcess fn [] Nothing Nothing (Just h) (Just h) (Just stderr)
          waitForProcess p
          return ()

