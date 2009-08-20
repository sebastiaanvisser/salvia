module Network.Protocol.Http.Printer () where {- doc ok -}

import Data.Bimap hiding (elems, fold)
import Data.Map
import Network.Protocol.Http.Data
import Network.Protocol.Http.Status

instance Show (Http Request) where
  showsPrec _ (Http (Request m u) v hs) =
      shows m . ss " " . ss u . ss " "
    . shows v . eol
    . shows hs . eol . eol

instance Show (Http Response) where
  showsPrec _ (Http (Response s) v hs) =
      shows v . ss " "
    . maybe (ss "Unknown status") shows (lookupR s statusCodes)
    . ss " " . shows s . eol
    . shows hs . eol

instance Show Headers where
  showsPrec _ =
      fold (\a b -> a . eol . b) id
    . mapWithKey (\k a -> ss k . ss ": " . ss a)
    . unHeaders

instance Show Version where
  showsPrec _ (Version a b) = ss "HTTP/" . shows a . ss "." . shows b

eol :: ShowS
eol = ss "\r\n"

ss :: String -> ShowS
ss = showString

