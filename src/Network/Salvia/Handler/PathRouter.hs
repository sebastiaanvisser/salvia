module Network.Salvia.Handler.PathRouter {- doc ok -}
  ( hPath
  , hPathRouter
  , hPrefix
  , hPrefixRouter

  , hParameters
  ) where

import Control.Applicative
import Data.List
import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Handler.Rewrite
import Network.Salvia.Handler.Dispatching
import Network.Salvia.Core.Aspects

{- | Request dispatcher based on the request path. -}

hPath :: RequestM m => Dispatcher String m a
hPath p h = hRequestDispatch (path % asURI) (==) p (chop p h)

{- | List dispatcher version of `hPath`. -}

hPathRouter :: RequestM m => ListDispatcher String m a
hPathRouter = hListDispatch hPath

{- | Request dispatcher based on a prefix of the request path. -}

hPrefix :: RequestM m => Dispatcher String m a
hPrefix p h = hRequestDispatch (path % asURI) isPrefixOf p (chop p h)

{- | List dispatcher version of `hPrefix`. -}

hPrefixRouter :: RequestM m => ListDispatcher String m a
hPrefixRouter = hListDispatch hPrefix

{- | Helper function to fetch the URI parameters from the request. -}

hParameters :: RequestM m => m Parameters
hParameters = queryParams <$> request (getM asURI)

-- todo: query routing
{- [(("this key", "isthisvalue"), myHandler)] -}


-- Helper.

chop :: RequestM m => String -> m a -> m a
chop a = hLocalRequest (path % asURI) (drop (length a))

