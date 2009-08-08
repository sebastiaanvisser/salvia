module Network.Salvia.Core.Config (
    HttpdConfig (..)
  , defaultConfig
  ) where

import Network.Socket hiding (send, listen)
import System.IO

{- |
The HTTP server configuration specifies some important network settings the
server must know before being able to run. Most fields speak for themselves.
-}

data HttpdConfig =
   HttpdConfig {
     hostname   :: String       -- ^ Server hostname.
   , email      :: String       -- ^ Server admin email address.
   , listenAddr :: HostAddress  -- ^ Addres to bind to.
   , listenPort :: PortNumber   -- ^ Port to listen on.
   , backlog    :: Int          -- ^ TCP backlog.
   , bufferSize :: Int          -- ^ Serve chunck with size.
   }

{- |
The default server configuration sets some safe default values. The server will
by default bind to 0.0.0.0 at port 80. The default value for the TCP backlog is
4, the default socket buffer size is 64KB. This function has to be in IO
because of the translation from a `String` to a `HostAddress` using
`inet_addr`.
-}

defaultConfig :: IO HttpdConfig
defaultConfig = do
  addr <- inet_addr "0.0.0.0"
  return
    $ HttpdConfig {
      hostname   = "hostname"
    , email      = "admin@localhost"
    , listenAddr = addr
    , listenPort = 80
    , backlog    = 4
    , bufferSize = 64 * 1024
    }
