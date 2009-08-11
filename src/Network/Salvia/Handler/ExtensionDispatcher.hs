module Network.Salvia.Handler.ExtensionDispatcher (
    hExtension
  , hExtensionRouter
  ) where

import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Handler.Dispatching
import Network.Salvia.Core.Aspects

{- | Request dispatcher based on the request path file extenstion. -}

hExtension :: Request m => Maybe String -> m a -> m a -> m a
hExtension = hRequestDispatch (extension % path % asURI) (==)

{- | List dispatcher version of `hExtension`. -}

hExtensionRouter :: Request m => [(Maybe String, m a)] -> m a -> m a
hExtensionRouter = hListDispatch hExtension

