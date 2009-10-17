module Network.Salvia.Handler.Path
  ( hPath
  , hPathRouter
  , hPrefix
  , hPrefixRouter

  , hQueryParameters
  )
where

import Control.Category
import Data.List
import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Core.Aspects
import Network.Salvia.Handler.Dispatching
import Network.Salvia.Handler.Rewrite
import Prelude hiding ((.), id)

{- | Request dispatcher based on the request path. -}

hPath :: HttpM Request m => Dispatcher String m a
hPath p h = hRequestDispatch (path . asUri) (==) p (chop p h)

{- | List dispatcher version of `hPath`. -}

hPathRouter :: HttpM Request m => ListDispatcher String m a
hPathRouter = hListDispatch hPath

{- | Request dispatcher based on a prefix of the request path. -}

hPrefix :: HttpM Request m => Dispatcher String m a
hPrefix p h = hRequestDispatch (path . asUri) isPrefixOf p (chop p h)

{- | List dispatcher version of `hPrefix`. -}

hPrefixRouter :: HttpM Request m => ListDispatcher String m a
hPrefixRouter = hListDispatch hPrefix

{- | Helper function to fetch the URI parameters from the request. -}

hQueryParameters :: HttpM Request m => m Parameters
hQueryParameters = request (getM (queryParams . asUri))

-- Helper.

chop :: HttpM Request m => String -> m a -> m a
chop a = hLocalRequest (path . asUri) (drop (length a))

