module Network.Salvia.Handler.Printer (hPrinter) where

import Network.Salvia.Httpd

{- |
The 'hPrinter' handler print the entire response including the headers to the
client. This handler is generally used as (one of) the last handler in a
handler environment.
-}

hPrinter :: Socket m => m ()
hPrinter = flushHeaders >> flushQueue

