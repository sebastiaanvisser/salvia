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

hExtension :: HttpM Request m => Maybe String -> m a -> m a -> m a
hExtension = hRequestDispatch (extension . path . asUri) (==)

{- | List dispatcher version of `hExtension`. -}

hExtensionRouter :: HttpM Request m => [(Maybe String, m a)] -> m a -> m a
hExtensionRouter = hListDispatch hExtension

