{-# LANGUAGE  FlexibleInstances #-}
module Network.Protocol.Http.Printer () where {- doc ok -}

import Data.Bimap hiding (elems, fold)
import Data.Map
import Network.Protocol.Http.Data
import Network.Protocol.Http.Status

instance Show (HTTP Request) where
  showsPrec _ (HTTP (Request m u) v hs) =
      shows m . sh " " . sh u . sh " "
    . shows v . eol
    . shows hs . eol . eol

instance Show (HTTP Response) where
  showsPrec _ (HTTP (Response s) v hs) =
      shows v . sh " "
    . maybe (sh "Unknown status") shows (lookupR s statusCodes)
    . sh " " . shows s . eol
    . shows hs . eol . eol

instance Show Headers where
  showsPrec _ =
      fold (\a b -> a . eol . b) id
    . mapWithKey (\k a -> sh k . sh ": " . sh a)
    . unHeaders

instance Show Version where
  showsPrec _ (Version a b) = sh "HTTP/" . shows a . sh "." . shows b

eol :: ShowS
eol = sh "\r\n"

sh :: String -> ShowS
sh = showString

