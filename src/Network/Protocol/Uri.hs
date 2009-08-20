{- | See rfc2396 for more info. -}

{-# LANGUAGE TemplateHaskell #-}
module Network.Protocol.Uri (

    Scheme
  , RegName
  , Port
  , Query
  , Fragment
  , Hash
  , UserInfo
  , PathSegment
  , Parameters

  , Domain (..)
  , IPv4 (..)
  , Path (..)
  , Host (..)
  , Authority (..)
  , URI (..)

  -- * Creating (parts of) URIs.

  , encode
  , decode

  , mkURI
  , mkScheme
  , mkPath
  , mkAuthority
  , mkQuery
  , mkFragment
  , mkUserinfo
  , mkHost
  , mkPort

  -- * Accessing parts of URIs.

  , domain
  , regname
  , ipv4
  , userinfo
  , host
  , port
  , relative
  , scheme
  , authority
  , path
  , query
  , fragment

  -- * Helper labels.

  , segments
  , _host
  , _port

  -- * Parsing URIs.

  , toURI
  , parseURI
  , parseAbsoluteURI
  , parseAuthority
  , parsePath
  , parseHost

  , pUriReference
  , pAbsoluteURI
  , pAuthority
  , pPath
  , pHost

  -- * Handling query parameters.

  , parseQueryParams
  , queryParams

  -- * Filename related utilities.

  , extension

  , mkPathRelative
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

