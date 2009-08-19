module Network.Salvia.Handler.Put (hPut) where {- todo: doc + impl -}

import Control.Monad.State
import Network.Protocol.Http
import Network.Salvia.Core.Aspects
import Network.Salvia.Handler.Error
import System.IO
import qualified Data.ByteString.Lazy as B

-- todo:     hPut = hMethod PUT hStore ...

-- | First naive handler for the HTTP `PUT` request. 

hPut
  :: (MonadIO m, HttpM Response m, SendM m, BodyM d m)
  => d -> FilePath -> m ()
hPut d name =
  hSafeIO (openBinaryFile name WriteMode)
    $ (body d >>=) . maybe putError . putOk
  where
    putError   = hError NotImplemented
    putOk fd c = liftIO (B.hPut fd c >> hClose fd)

