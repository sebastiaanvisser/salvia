{-# LANGUAGE TypeOperators #-}
module Network.Salvia.Handler.Rewrite {- doc ok -}
  ( hLocalRequest

  , hRewrite
  , hRewritePath
  , hRewriteHost
  , hRewriteExt

  , hWithDir
  , hWithoutDir

  ) where

import Control.Applicative
import Data.List
import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Core.Aspects

{- |
Run a handler in a local environment in which the `HTTP' `Request' has
been modified.
-}

hLocalRequest :: RequestM m => (HTTP Request :-> b) -> (b -> b) -> m a -> m a
hLocalRequest p f m =
  do u <- request (getM p) <* request (modM p f)
     m <* request (setM p u)

{- |
Run an handler in a modified context in which the request `URI` has been
changed by the specified modifier function. After the handler completes the `URI`
remains untouched.
-}

hRewrite :: RequestM m => (URI -> URI) -> m a -> m a
hRewrite = hLocalRequest asURI

{- | Run handler in a context with a modified host. -}

hRewriteHost :: RequestM m => (String -> String) -> m a -> m a
hRewriteHost = hLocalRequest (host % asURI)

{- | Run handler in a context with a modified path. -}

hRewritePath :: RequestM m => (FilePath -> FilePath) -> m a -> m a
hRewritePath = hLocalRequest (path % asURI)

{- | Run handler in a context with a modified file extension. -}

hRewriteExt :: RequestM m => (Maybe String -> Maybe String) -> m a -> m a
hRewriteExt = hLocalRequest (extension % path % asURI)

{- |
Run handler in a context with a modified path. The specified prefix will be
prepended to the path.
-}

hWithDir :: RequestM m => String -> m a -> m a
hWithDir d = hRewritePath (d++)

{- |
Run handler in a context with a modified path. The specified prefix will be
stripped from the path.
-}

hWithoutDir :: RequestM m => String -> m a -> m a
hWithoutDir d = hRewritePath $
  \p -> if d `isPrefixOf` p then drop (length d) p else p

