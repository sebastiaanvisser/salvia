module Network.Salvia.Handler.Put (hPut) where {- doc ok -}

import Control.Monad.State
import Network.Protocol.Http
import Network.Salvia.Core.Aspects
import Network.Salvia.Handler.Error
import System.IO
import qualified Data.ByteString.Lazy as B

-- todo:     hPut = hMethod PUT hStore ...

-- | First naive handler for the HTTP `PUT` request. 

hPut :: (MonadIO m, ResponseM m, SendM m, ContentsM m) => FilePath -> m ()
hPut name =
  hSafeIO (openBinaryFile name WriteMode)
    $ (contents >>=) . maybe putError . putOk
  where
    putError   = hError NotImplemented
    putOk fd c = liftIO (B.hPut fd c >> hClose fd)

