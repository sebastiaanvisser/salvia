module Network.Salvia.Core.Config where

import Network.Socket

{- |
The HTTP server configuration contains some network settings the server needs
know before being able to run. Most fields speak for themselves.
-}

data Config =
  Config
  { hostname :: String       -- ^ Server hostname.
  , admin    :: String       -- ^ Server admin email address.
  , listenOn :: [SockAddr]   -- ^ Address port combinations to listen on.
  , backlog  :: Int          -- ^ TCP backlog.
  }

{- |
The default server configuration sets some safe default values. The server will
by default bind to 0.0.0.0 (`iNADDR_ANY') at port 8080. The default value for
the TCP backlog is 64.
-}

defaultConfig :: Config
defaultConfig =
  Config
  { hostname = "localhost"
  , admin    = "admin@localhost"
  , listenOn = [SockAddrInet 8080 iNADDR_ANY]
  , backlog  = 64
  }

