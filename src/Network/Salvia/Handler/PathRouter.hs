module Network.Salvia.Handler.PathRouter (
    hPath
  , hPathRouter
  , hPrefix
  , hPrefixRouter

  , hParameters
  ) where

import Control.Monad.State
import Data.List (isPrefixOf)
import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Uri (path, queryParams, Parameters)
import Network.Salvia.Handler.Rewrite
import Network.Salvia.Handler.Dispatching
import Network.Salvia.Core.Aspects

{- | Request dispatcher based on the request path. -}

hPath :: Request m => Dispatcher String m a
hPath p h = hRequestDispatch (path % asURI) (==) p (chop p h)

{- | List dispatcher version of `hPath`. -}

hPathRouter :: Request m => ListDispatcher String m a
hPathRouter = hListDispatch hPath

{- | Request dispatcher based on a prefix of the request path. -}

hPrefix :: Request m => Dispatcher String m a
hPrefix p h = hRequestDispatch (path % asURI) isPrefixOf p (chop p h)

{- | List dispatcher version of `hPrefix`. -}

hPrefixRouter :: Request m => ListDispatcher String m a
hPrefixRouter = hListDispatch hPrefix

{- | Helper function to fetch the URI parameters from the request. -}

-- hParameters :: Handler Parameters

hParameters :: Request m => m Parameters
hParameters = queryParams `liftM` request (getM asURI)

chop :: Request m => String -> m a -> m a
chop a = hLocalRequest (path % asURI) (drop (length a))

