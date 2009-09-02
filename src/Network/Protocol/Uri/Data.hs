{-# LANGUAGE TemplateHaskell #-}
module Network.Protocol.Uri.Data where

import Prelude hiding ((.), id)
import Control.Category
import Data.Record.Label
import Data.Maybe
import Network.Protocol.Uri.Encode

type Scheme      = String
type RegName     = String
type Port        = Int
type Query       = String
type Fragment    = String
type Hash        = String
type UserInfo    = String
type PathSegment = String

data IPv4 = IPv4 Int Int Int Int
  deriving (Eq, Ord)

newtype Domain = Domain { __parts :: [String] }
  deriving (Eq, Ord)

data Host =
    Hostname { __domain  :: Domain }
  | RegName  { __regname :: RegName }
  | IP       { __ipv4    :: IPv4   }
--  | IPv6     { __ipv6    :: IPv6   }
  deriving (Eq, Ord)

data Authority = Authority
  { __userinfo :: UserInfo
  , __host     :: Host
  , __port     :: Maybe Port
  }
  deriving (Eq, Ord)

data Path = Path { __segments :: [PathSegment] }
  deriving (Eq, Ord)

data Uri = Uri
  { _relative  :: Bool
  , _scheme    :: Scheme
  , _authority :: Authority
  , __path     :: Path
  , __query    :: Query
  , __fragment :: Fragment
  }
  deriving (Eq, Ord)

$(mkLabels [''Domain, ''Path, ''Host, ''Authority, ''Uri])

_domain   :: Host :-> Domain
_ipv4     :: Host :-> IPv4
_regname  :: Host :-> String
_host     :: Authority :-> Host
_port     :: Authority :-> Maybe Port
_userinfo :: Authority :-> UserInfo
_segments :: Path :-> [PathSegment]
_path     :: Uri :-> Path

-- | Access raw (URI-encoded) query.

_query :: Uri :-> Query

-- | Access authority part of the URI.

authority :: Uri :-> Authority

-- | Access domain part of the URI, returns `Nothing' when the host is a
-- regname or IP-address.

domain :: Uri :-> Maybe Domain
domain = (f <-> Hostname . fromJust) `iso` (_host . authority)
  where
    f (Hostname d) = Just d
    f _            = Nothing

-- | Access regname part of the URI, returns `Nothing' when the host is a
-- domain or IP-address.

regname :: Uri :-> Maybe RegName
regname = (f <-> RegName . fromJust) `iso` (_host . authority)
  where
    f (RegName r) = Just r
    f _           = Nothing

-- | Access IPv4-address part of the URI, returns `Nothing' when the host is a
-- domain or regname.

ipv4 :: Uri :-> Maybe IPv4
ipv4 = (f <-> IP . fromJust) `iso` (_host . authority)
  where
    f (IP i) = Just i
    f _      = Nothing

-- | Access raw (URI-encoded) fragment.

_fragment :: Uri :-> Fragment

-- | Access the port number part of the URI when available.

port :: Uri :-> Maybe Port
port = _port . authority

-- | Access the query part of the URI, the part that follows the ?. The query
-- will be properly decoded when reading and encoded when writing.

query :: Uri :-> Query
query = encoded `iso` _query

-- | Access the fragment part of the URI, the part that follows the #. The
-- fragment will be properly decoded when reading and encoded when writing.

fragment :: Uri :-> Fragment
fragment = encoded `iso` _fragment

-- | Is a URI relative?

relative :: Uri :-> Bool

-- | Access the scheme part of the URI. A scheme is probably the protocol
-- indicator like /http/, /ftp/, etc.

scheme :: Uri :-> Scheme

-- | Access the path part of the URI as a list of path segments. The segments
-- will still be URI-encoded.

segments :: Uri :-> [PathSegment]
segments = _segments . _path

-- | Access the userinfo part of the URI. The userinfo contains an optional
-- username and password or some other credentials.

userinfo :: Uri :-> String
userinfo = _userinfo . authority

-- | Constructors for making empty URI.

mkUri :: Uri
mkUri = Uri False mkScheme mkAuthority mkPath mkQuery mkFragment

-- | Constructors for making empty `Scheme`.

mkScheme :: Scheme
mkScheme = ""

-- | Constructors for making empty `Path`.

mkPath :: Path
mkPath = Path []

-- | Constructors for making empty `Authority`.

mkAuthority :: Authority
mkAuthority = Authority "" mkHost mkPort

-- | Constructors for making empty `Query`.

mkQuery :: Query
mkQuery = ""

-- | Constructors for making empty `Fragment`.

mkFragment :: Fragment
mkFragment = ""

-- | Constructors for making empty `UserInfo`.

mkUserinfo :: UserInfo
mkUserinfo = ""

-- | Constructors for making empty `Host`.

mkHost :: Host
mkHost = Hostname (Domain [])

-- | Constructors for making empty `Port`.

mkPort :: Maybe Port
mkPort = Nothing

