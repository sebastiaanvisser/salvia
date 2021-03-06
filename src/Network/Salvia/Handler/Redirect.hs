{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.Redirect (hRedirect) where

import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Interface

{- |
Redirect a client to another location by creating a `MovedPermanently` response
message with the specified `URI` in the `location' header.
-}

hRedirect :: HttpM Response m => String -> m ()
hRedirect u =
  response $
    do location =: Just u
       status   =: MovedPermanently

