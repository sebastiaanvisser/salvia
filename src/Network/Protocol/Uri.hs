{- | See rfc2396 for more info. -}

{-# LANGUAGE TemplateHaskell #-}
module Network.Protocol.Uri (

  -- * URI datatype.

    Scheme
  , RegName
  , Port
  , Query
  , Fragment
  , Hash
  , UserInfo
  , PathSegment
  , Parameters

  , Domain (Domain)
  , IPv4 (IPv4)
  , Path (Path)
  , Host (Hostname, RegName, IP)
  , Authority (Authority)
  , Uri (Uri)

  -- * Accessing parts of URIs.

  , relative
  , scheme
  , userinfo
  , authority
  , host
  , domain
  , ipv4
  , regname
  , port
  , path
  , segments
  , query
  , fragment

  -- * More advanced labels.

  , queryParams
  , params
  , extension

  -- * Encoding/decoding URI encoded strings.

  , encode
  , decode
  , encoded

  -- * Creating empty URIs.

  , mkUri
  , mkScheme
  , mkPath
  , mkAuthority
  , mkQuery
  , mkFragment
  , mkUserinfo
  , mkHost
  , mkPort

  -- * Parsing URIs.

  , toUri
  , parseUri
  , parseAbsoluteUri
  , parseAuthority
  , parsePath
  , parseHost

  -- * Filename related utilities.

  , mimetype
  , normalize
  , jail
  , (/+)

  ) where

import Network.Protocol.Uri.Data
import Network.Protocol.Uri.Encode
import Network.Protocol.Uri.Parser
import Network.Protocol.Uri.Path
import Network.Protocol.Uri.Printer ()
import Network.Protocol.Uri.Query

