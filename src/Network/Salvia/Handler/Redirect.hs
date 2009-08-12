module Network.Salvia.Handler.Redirect (hRedirect) where {- doc ok -}

import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Core.Aspects

{- |
Redirect a client to another location by creating a `MovedPermanently` response
message with the specified `URI` in the `location' header.
-}

hRedirect :: ResponseM m => String -> m ()
hRedirect u =
  response $
    do location =: Just u
       status   =: MovedPermanently

