module Misc.Util where

import Control.Monad.Trans

catchIO :: MonadIO m => IO a -> a -> m a
catchIO a b = liftIO (a `catch` (const (return b)))

