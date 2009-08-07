module Network.Salvia.Handler.ExtensionDispatcher (
    hExtension
  , hExtensionRouter
  ) where

import Data.Record.Label
import Network.Protocol.Http (uri)
import Network.Protocol.Uri (extension, path)
import Network.Salvia.Handler.Dispatching
import Network.Salvia.Httpd

{- | Request dispatcher based on the request path file extenstion. -}

hExtension :: Request m => Maybe String -> m a -> m a -> m a
hExtension = hRequestDispatch (extension % path % uri) (==)

{- | List dispatcher version of `hExtension`. -}

hExtensionRouter :: Request m => [(Maybe String, m a)] -> m a -> m a
hExtensionRouter = hListDispatch hExtension

