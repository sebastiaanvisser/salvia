module Network.Salvia.Handler.VirtualHosting {- doc ok -}
  ( hVirtualHosting
  , hPortRouter
  )
where

import Data.List.Split
import Data.List
import Data.Maybe
import Network.Protocol.Uri
import Network.Protocol.Http
import Network.Salvia.Handler.Dispatching
import Network.Salvia.Core.Aspects

{- |
Dispatcher based on the host part of the `hostname' request header. Everything
not part of the real hostname (like the port number) will be ignored. When the
expected hostname starts with a dot (like ".mydomain.com")  this indicates that
all sub-domains of this domain will match as well.
-}

hVirtualHosting :: HttpM Request m => ListDispatcher String m b
hVirtualHosting = hListDispatch (hRequestDispatch hostname (\a -> False `maybe` (match a)))
  where
  match e f = 
    case parseAuthority f of
      Right (Authority _ hst _) -> 
        case (e, hst) of
          ('.':_, Hostname (Domain d)) -> filter (not . null) (splitOn "." e) `isSuffixOf` d
          (_,     Hostname d)          -> e == show d
          (_,     RegName r)           -> e == r
          (_,     IP i)                -> e == show i
      _ -> False

{- |
Dispatcher based on the port number of the `hostname' request header. When no
port number is available in the hostname header port 80 will be assumed.
-}

hPortRouter :: HttpM Request m => ListDispatcher Int m b
hPortRouter = hListDispatch (hRequestDispatch hostname (\a -> False `maybe` (match a)))
  where
  match e f = 
    case parseAuthority f of
      Right (Authority _ _ prt) -> fromMaybe 80 prt == e
      _                         -> False

