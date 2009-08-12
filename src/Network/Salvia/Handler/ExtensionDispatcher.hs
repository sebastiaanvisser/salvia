module Network.Salvia.Handler.ExtensionDispatcher {- doc ok -}
  ( hExtension
  , hExtensionRouter
  ) where

import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Handler.Dispatching
import Network.Salvia.Core.Aspects

{- | Request dispatcher based on the request path file extenstion. -}

hExtension :: RequestM m => Maybe String -> m a -> m a -> m a
hExtension = hRequestDispatch (extension % path % asURI) (==)

{- | List dispatcher version of `hExtension`. -}

hExtensionRouter :: RequestM m => [(Maybe String, m a)] -> m a -> m a
hExtensionRouter = hListDispatch hExtension

