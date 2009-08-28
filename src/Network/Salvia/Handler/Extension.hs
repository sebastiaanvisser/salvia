module Network.Salvia.Handler.Extension {- doc ok -}
  ( hExtension
  , hExtensionRouter
  )
where

import Control.Category
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Handler.Dispatching
import Network.Salvia.Core.Aspects
import Prelude hiding ((.), id)

{- | Request dispatcher based on the request path file extenstion. -}

hExtension :: HttpM Request m => Maybe String -> m a -> m a -> m a
hExtension = hRequestDispatch (extension . path . asUri) (==)

{- | List dispatcher version of `hExtension`. -}

hExtensionRouter :: HttpM Request m => [(Maybe String, m a)] -> m a -> m a
hExtensionRouter = hListDispatch hExtension

