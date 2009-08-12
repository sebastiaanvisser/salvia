module Network.Salvia.Handler.VirtualHosting {- todo -}
  ( -- hHostRouter
    hVirtualHosting
  ) where

import Network.Protocol.Http
import Network.Salvia.Handler.Dispatching
import Network.Salvia.Core.Aspects

-- todo: add PortRouter.

{- |
List dispatcher based on the host part of the hostname request header.
Everything not part of the real hostname (like the port number) will be
ignored.
-}

hVirtualHosting :: RequestM m => [(String, m b)] -> m b -> m b
hVirtualHosting = hListDispatch (hRequestDispatch hostname ((==) . Just))

{-hVirtualHosting :: RequestM m => [(String, m b)] -> m b -> m b
hVirtualHosting = hListDispatch disp . parse
  where
    disp    = hRequestDispatch hostname cmp
    parse   = map (\(a, b) -> (either (const mkAuthority) id $ parseAuthority a, b))
    cmp a b = (==EQ) $ comparing (lget _host) a b-}

{- |
List dispatcher based on the hostname request header. This header field is
parsed and interpreted as an `Authority` field.
-}

-- hHostRouter :: RequestM m => [(Authority, m b)] -> m b -> m b
-- hHostRouter = hListDispatch $ hRequestDispatch hostname (==)

