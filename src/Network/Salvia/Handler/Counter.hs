module Network.Salvia.Handler.Counter (hCounter) where {- doc ok -}

import Control.Concurrent.STM
import Control.Monad.State

{- | This handler simply increases the request counter variable. -}

hCounter :: MonadIO m => TVar Int -> m ()
hCounter c = liftIO $ atomically $ readTVar c >>= writeTVar c . (+1)

