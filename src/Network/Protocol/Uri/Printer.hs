module Network.Protocol.Uri.Printer where

import Data.List
import Network.Protocol.Uri.Data
import Misc.Text

instance Show Path where
  showsPrec _ (Path ("":xs)) = sc '/' . shows (Path xs)
  showsPrec _ (Path xs)      = intersperseS (sc '/') (map ss xs)

instance Show IPv4 where
  showsPrec _ (IPv4 a b c d) = intersperseS (sc ',') (map shows [a, b, c, d])

instance Show Domain where
  showsPrec _ (Domain d) = intersperseS (sc '.') (map ss d)

instance Show Host where
  showsPrec _ (Hostname d) = shows d
  showsPrec _ (IP i)       = shows i 
  showsPrec _ (RegName r)  = ss r

instance Show Authority where
  showsPrec _ (Authority u h p) =
    let u' = if null u then id else ss u . ss "@"
        p' = maybe id (\s -> sc ':' . shows s) p
    in u' . shows h . p'

instance Show URI where
  showsPrec _ (URI _ s a p q f) =
    let s' = if null s then id else ss s . sc ':'
        a' = show a
        p' = shows p
        q' = if null q then id else sc '?' . ss q
        f' = if null f then id else sc '#' . ss f
        t' = if null a' then id else ss "//"
    in s' . t' . ss a' . p' . q' . f'

ss :: String -> ShowS
ss = showString

sc :: Char -> ShowS
sc = showChar

