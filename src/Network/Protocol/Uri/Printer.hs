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

instance Show Host where
  show (Hostname d) = if null d then "" else intercalate "." d
  show (IPv4     a) = intercalate "." $ map show a
  show (RegName  r) = r

instance Show Authority where
  show (Authority u h p) =
--        (if hst h then "//" else "")
       (if null u then "" else u ++ "@")
    ++  show h
    ++ (if p == -1 then "" else ":" ++ show p)
--     where hst (Hostname d) = not $ null d
--           hst (RegName  d) = not $ null d
--           hst _            = True

instance Show URI where
  show (URI r s a p q f) =
       (if not r then (if null s then "" else s ++ ":") else "")
    ++ (show a) ++ "//"
    ++ (show p)
    ++ (if null q then "" else "?" ++ q)
    ++ (if null f then "" else "#" ++ f)

