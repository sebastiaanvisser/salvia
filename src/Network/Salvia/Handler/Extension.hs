{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.Extension
  ( hExtension
  , hExtensionRouter
  )
where

import Control.Category
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Handler.Dispatching
import Network.Salvia.Interface
import Prelude hiding ((.), id)

{- | Request dispatcher based on the request path file extenstion. -}

hExtension :: HttpM Request m => Dispatcher (Maybe String) m a
hExtension = hRequestDispatch (extension . path . asUri) (==)

{- | List dispatcher version of `hExtension`. -}

hExtensionRouter :: HttpM Request m => ListDispatcher (Maybe String) m a
hExtensionRouter = hListDispatch hExtension

