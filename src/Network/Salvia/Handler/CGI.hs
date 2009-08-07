module Network.Salvia.Handler.CGI (hCGI) where

import Control.Monad.State
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Httpd
import System.IO
import System.Process (runProcess, waitForProcess)

{- |
Handle CGI scripts, not yet working properly.
-}

hCGI :: (MonadIO m, Response m, Socket m) => FilePath -> m ()
hCGI name = do
  response (setM status OK)
  h <- sock
  liftIO $ do
    p <- runProcess name [] Nothing Nothing (Just h) (Just h) (Just stderr)
    waitForProcess p
    return ()

