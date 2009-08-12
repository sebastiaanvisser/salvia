module Network.Salvia.Handler.MethodRouter {- doc ok -}
  ( hMethod
  , hMethodRouter
  ) where

import Network.Protocol.Http
import Network.Salvia.Handler.Dispatching
import Network.Salvia.Core.Aspects

{- | Request dispatcher based on the HTTP request `Method`.  -}

hMethod :: RequestM m => Dispatcher Method m a
hMethod = hRequestDispatch method (==)

{- | Request list dispatcher based on the `hMethod` dispatcher. -}

hMethodRouter :: RequestM m => ListDispatcher Method m ()
hMethodRouter = hListDispatch hMethod

