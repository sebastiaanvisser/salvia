module Network.Salvia.Handler.Redirect (hRedirect) where

import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Uri (URI)
import Network.Salvia.Httpd

{- |
Redirect a client to another location by creating a `MovedPermanently` response
message with the specified `URI` in the `location' header.
-}

hRedirect :: Response m => URI -> m ()
hRedirect u =
  response $
    do setM location (Just u)
       setM status MovedPermanently

