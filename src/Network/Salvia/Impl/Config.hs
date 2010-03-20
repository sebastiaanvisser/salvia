module Network.Salvia.Impl.Config where

import Network.Socket

{- |
The HTTP server configuration contains some network settings the server needs
know before being able to run. 
-}

data Config =
  Config
  { hostname  :: String       -- ^ Server hostname.
  , adminMail :: String       -- ^ Server admin email address.
  , listenOn  :: [SockAddr]   -- ^ Address port combinations to listen on.
  , backlog   :: Int          -- ^ TCP backlog.
  }

{- |
The default server configuration sets some safe default values. The server will
by default bind to 0.0.0.0 (`iNADDR_ANY') at port 8080. The default value for
the TCP backlog is 64.
-}

defaultConfig :: Config
defaultConfig =
  Config
  { hostname  = "127.0.0.1"
  , adminMail = "admin@localhost"
  , listenOn  = [SockAddrInet 8080 iNADDR_ANY]
  , backlog   = 64
  }

