module Network.Protocol.Uri.Printer where

import Data.List
import Network.Protocol.Uri.Data

-- TODO: cleanup and test this mess: QuickCheck property for testing:
-- parse $ show u == u /\ show $ parse u' = u'

-- TODO: ShowS

instance Show Path where
  show (Path a s) =
       (if a then "/" else "")
    ++ (intercalate "/" s)

instance Show IPv4 where
  show (IPv4 a b c d) = intercalate "." (map show [a, b, c, d])

instance Show Domain where
  show (Domain d) = intercalate "." d

instance Show Host where
  show (Hostname d) = show d
  show (IP i)       = show i 
  show (RegName r)  = r

instance Show Authority where
  show (Authority u h p) = (if null u then "" else u ++ "@") ++ show h ++ maybe "" ((":" ++) . show) p

instance Show URI where
  show (URI r s a p q f) =
       (if not r then (if null s then "" else s ++ ":") else "")
    ++ (show a) ++ "//"
    ++ (show p)
    ++ (if null q then "" else "?" ++ q)
    ++ (if null f then "" else "#" ++ f)

