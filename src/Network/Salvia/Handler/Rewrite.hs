module Network.Salvia.Handler.Rewrite {- doc ok -}
  ( hLocalRequest

  , hRewrite
  , hRewritePath
  , hRewriteHost
  , hRewriteExt

  , hWithDir
  , hWithoutDir

  )
where

import Control.Applicative
import Control.Category
import Data.List
import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Core.Aspects
import Prelude hiding ((.), id)

{- |
Run a handler in a local environment in which the `HTTP' `Request' has
been modified.
-}

hLocalRequest :: HttpM Request m => (Http Request :-> b) -> (b -> b) -> m a -> m a
hLocalRequest p f m =
  do u <- request (getM p) <* request (modM p f)
     m <* request (p =: u)

{- |
Run an handler in a modified context in which the request `Uri` has been
changed by the specified modifier function. After the handler completes the `Uri`
remains untouched.
-}

hRewrite :: HttpM Request m => (Uri -> Uri) -> m a -> m a
hRewrite = hLocalRequest asUri

{- | Run handler in a context with a modified host. -}

hRewriteHost :: HttpM Request m => (String -> String) -> m a -> m a
hRewriteHost = hLocalRequest (host . asUri)

{- | Run handler in a context with a modified path. -}

hRewritePath :: HttpM Request m => (FilePath -> FilePath) -> m a -> m a
hRewritePath = hLocalRequest (path . asUri)

{- | Run handler in a context with a modified file extension. -}

hRewriteExt :: HttpM Request m => (Maybe String -> Maybe String) -> m a -> m a
hRewriteExt = hLocalRequest (extension . path . asUri)

{- |
Run handler in a context with a modified path. The specified prefix will be
prepended to the path.
-}

hWithDir :: HttpM Request m => String -> m a -> m a
hWithDir d = hRewritePath (d++)

{- |
Run handler in a context with a modified path. The specified prefix will be
stripped from the path.
-}

hWithoutDir :: HttpM Request m => String -> m a -> m a
hWithoutDir d = hRewritePath $
  \p -> if d `isPrefixOf` p then drop (length d) p else p

