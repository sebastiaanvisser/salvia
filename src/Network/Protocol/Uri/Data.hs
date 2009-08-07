{- | See rfc2396 for more info. -}

{-# LANGUAGE TemplateHaskell #-}
module Network.Protocol.Uri.Data where

import Data.Record.Label
import Network.Protocol.Uri.Encode

-------[ data type definition of URIs ]----------------------------------------

type Scheme      = String
type IPv4        = [Int] -- actually 4-tupel
type Domain      = [String]
type RegName     = String
type Port        = Int
type Query       = String
type Fragment    = String
type Hash        = String
type UserInfo    = String
type PathSegment = String

data Path = Path 
  { _absolute :: Bool
  , _segments :: [PathSegment]
  }
  deriving (Eq, Ord)

data Host =
    Hostname { __domain  :: Domain }
  | RegName  { __regname :: RegName }
  | IPv4     { __ipv4    :: IPv4   }
  deriving (Eq, Ord)

data Authority = Authority
  { __userinfo :: UserInfo
  , __host     :: Host
  , __port     :: Port
  }
  deriving (Eq, Ord)

data URI = URI
  { _relative  :: Bool
  , _scheme    :: Scheme
  , _authority :: Authority
  , __path     :: Path
  , __query     :: Query
  , _fragment  :: Fragment
  }
  deriving (Eq, Ord)

$(mkLabels [''Path, ''Host, ''Authority, ''URI])

_domain   :: Label Host Domain
_ipv4     :: Label Host IPv4
_regname  :: Label Host String
_host     :: Label Authority Host
_port     :: Label Authority Port
_userinfo :: Label Authority UserInfo
_path     :: Label URI Path
_query    :: Label URI Query

absolute  :: Label Path Bool
segments  :: Label Path [PathSegment]
authority :: Label URI Authority
domain    :: Label URI Domain
fragment  :: Label URI Fragment
ipv4      :: Label URI IPv4
port      :: Label URI Port
query     :: Label URI Query
regname   :: Label URI String
relative  :: Label URI Bool
scheme    :: Label URI Scheme
userinfo  :: Label URI UserInfo

-- Public label based on private labels.

domain    = _domain   % _host % authority
regname   = _regname  % _host % authority
ipv4      = _ipv4     % _host % authority
userinfo  = _userinfo % authority
port      = _port     % authority

query = mkLabel
  (decode . lget _query)
  (lset _query . encode)

-------[ creating, selection and modifying URIs ]------------------------------

{- | Constructors for making empty URI. -}

mkURI :: URI
mkURI = URI False mkScheme mkAuthority mkPath mkQuery mkFragment

{- | Constructors for making empty `Scheme`. -}

mkScheme :: Scheme
mkScheme = ""

{- | Constructors for making empty `Path`. -}

mkPath :: Path
mkPath = Path False []

{- | Constructors for making empty `Authority`. -}

mkAuthority :: Authority
mkAuthority = Authority "" mkHost mkPort

{- | Constructors for making empty `Query`. -}

mkQuery :: Query
mkQuery = ""

{- | Constructors for making empty `Fragment`. -}

mkFragment :: Fragment
mkFragment = ""

{- | Constructors for making empty `UserInfo`. -}

mkUserinfo :: UserInfo
mkUserinfo = ""

{- | Constructors for making empty `Host`. -}

mkHost :: Host
mkHost = Hostname []

{- | Constructors for making empty `Port`. -}

mkPort :: Port
mkPort = (-1)

