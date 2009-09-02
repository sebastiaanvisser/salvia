module Network.Protocol.Uri.Path where

import Data.List
import Network.Protocol.Mime
import Data.Record.Label

{- | Label to access the extension of a filename. -}

extension :: FilePath :-> Maybe String
extension = label getExt setExt
  where
    splt     p = (\(a,b) -> (reverse a, reverse b)) $ break (=='.') $ reverse p
    isExt e  p = '/' `elem` e || not ('.' `elem` p)
    getExt   p = let (u, v) = splt p in
                 if isExt u v then Nothing else Just u
    setExt e p = let (u, v) = splt p in
                 (if isExt u v then p else init v) ++ maybe "" ('.':) e

{- |
Try to guess the correct mime type for the input file based on the file
extension.
-}

mimetype :: FilePath -> Maybe String
mimetype p = get extension p >>= mime

{- |
Normalize a path by removing or merging all dot or dot-dot segments and double
slashes. 
-}

-- Todo: is this windows-safe?  is it really secure?

normalize :: FilePath -> FilePath
normalize p  = norm_rev (reverse p)
  where
    norm_rev ('/':t) = start_dir 0 "/" t
    norm_rev (    t) = start_dir 0 ""  t

    start_dir n q (".."           ) = rest_dir   n    q  ""
    start_dir n q ('/':t          ) = start_dir  n    q  t
    start_dir n q ('.':'/':t      ) = start_dir  n    q  t
    start_dir n q ('.':'.':'/': t ) = start_dir (n+1) q  t
    start_dir n q (t              ) = rest_dir   n    q  t

    rest_dir  n q  ""
        | n > 0      = foldr (++) q (replicate n "../")
        | null q     = "/"
        | otherwise  = q
    rest_dir  0 q ('/':t ) = start_dir  0   ('/':q)  t
    rest_dir  n q ('/':t ) = start_dir (n-1)     q   t
    rest_dir  0 q (h:t   ) = rest_dir   0   (  h:q)  t
    rest_dir  n q (_:t   ) = rest_dir   n        q   t

{- | Jail a filepath within a jail directory. -}

jail
  :: FilePath         -- ^ Jail directory.
  -> FilePath         -- ^ Filename to jail.
  -> Maybe FilePath
jail jailDir p =
  let nj = normalize jailDir
      np = normalize p in
  if nj `isPrefixOf` np -- && not (".." `isPrefixOf` np)
    then Just np
    else Nothing

{- | Concatenate and normalize two filepaths. -}

(/+) :: FilePath -> FilePath -> FilePath
a /+ b = normalize (a ++ "/" ++ b)

