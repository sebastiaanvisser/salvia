module Network.Salvia.Core.Config
  ( Config (..)
  , defaultConfig
  )
where

import Network.Socket

{- |
The HTTP server configuration contains some network settings the server needs
know before being able to run. Most fields speak for themselves.
-}

data Config =
  Config
    { hostname   :: String       -- ^ Server hostname.
    , admin      :: String       -- ^ Server admin email address.
    , listenAddr :: HostAddress  -- ^ Addres to bind to.
    , listenPort :: PortNumber   -- ^ Port to listen on.
    , backlog    :: Int          -- ^ TCP backlog.
    }

{- |
The default server configuration sets some safe default values. The server will
by default bind to 0.0.0.0 at port 80. The default value for the TCP backlog is
64, the default socket buffer size is 64KB.
-}

defaultConfig :: Config
defaultConfig =
  Config
    { hostname   = "localhost"
    , admin      = "admin@localhost"
    , listenAddr = 0
    , listenPort = 80
    , backlog    = 64
    }

