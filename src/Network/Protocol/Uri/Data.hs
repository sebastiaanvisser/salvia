{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Network.Protocol.Uri.Data where

import Data.Record.Label
import Network.Protocol.Uri.Encode

{- | See rfc2396 for more info. -}

--- URI data type definition. 

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

_domain   :: Host :-> Domain
_ipv4     :: Host :-> IPv4
_regname  :: Host :-> String
_host     :: Authority :-> Host
_port     :: Authority :-> Port
_userinfo :: Authority :-> UserInfo
_path     :: URI :-> Path
_query    :: URI :-> Query

absolute  :: Path :-> Bool
segments  :: Path :-> [PathSegment]
authority :: URI :-> Authority
domain    :: URI :-> Domain
fragment  :: URI :-> Fragment
ipv4      :: URI :-> IPv4
port      :: URI :-> Port
query     :: URI :-> Query
regname   :: URI :-> String
relative  :: URI :-> Bool
scheme    :: URI :-> Scheme
userinfo  :: URI :-> UserInfo

-- Public label based on private labels.

domain    = _domain   % _host % authority
regname   = _regname  % _host % authority
ipv4      = _ipv4     % _host % authority
userinfo  = _userinfo % authority
port      = _port     % authority
query     = (decode, encode) `lmap` _query

-- Creating, selection and modifying URIs.

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

