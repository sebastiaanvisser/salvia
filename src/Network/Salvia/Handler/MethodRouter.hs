module Network.Salvia.Handler.MethodRouter (
    hMethod
  , hMethodRouter
  ) where

import Network.Protocol.Http
import Network.Salvia.Handler.Dispatching
import Network.Salvia.Httpd

{- | Request dispatcher based on the HTTP request `Method`.  -}

hMethod :: Request m => Dispatcher Method m a
hMethod = hRequestDispatch method (==)

{- | Request list dispatcher based on the `hMethod` dispatcher. -}

hMethodRouter :: Request m => ListDispatcher Method m ()
hMethodRouter = hListDispatch hMethod

