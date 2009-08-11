module Network.Salvia.Handler.Printer (
    hRequestPrinter
  , hResponsePrinter
  ) where

import Network.Salvia.Core.Aspects

{- |
The 'hPrinter' handler print the entire response including the headers to the
client. This handler is generally used as (one of) the last handler in a
handler environment.
-}

hRequestPrinter :: Send m => m ()
hRequestPrinter = flushRequest >> flushQueue

hResponsePrinter :: Send m => m ()
hResponsePrinter = flushResponse >> flushQueue

