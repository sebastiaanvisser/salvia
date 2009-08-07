{- | Provides the `Show` instance for an HTTP `Message`. -}

module Network.Protocol.Http.Printer (showMessageHeader) where

import Data.Bimap (lookupR)
import Data.List (intercalate)
import Data.Map (elems, mapWithKey)
import Data.Record.Label (lget)
import Network.Protocol.Http.Data
import Network.Protocol.Http.Status

lf :: String
lf = "\r\n"

instance Show Version where
  show (Version a b) = concat ["HTTP/", show a, ".", show b]

showHeaders :: Headers -> String
showHeaders =
    intercalate lf
  . elems
  . mapWithKey (\k a -> k ++ ": " ++ a)

{- | Helper function that only prints the header part of an HTTP message. -}

showMessageHeader :: Message -> String
showMessageHeader (Message (Response s) v hs _) =
  concat [
    show v, " "
  , maybe "Unknown status" show
  $ lookupR s statusCodes, " "
  , show s, lf
  , showHeaders hs, lf, lf
  ]

showMessageHeader (Message (Request m u) v hs _) =
  concat [show m, " ", show u, " ", show v, lf, showHeaders hs, lf, lf]

instance Show Message where
  show m = concat [showMessageHeader m, lget body m]

