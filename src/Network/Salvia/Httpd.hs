module Network.Salvia.Httpd
( Config (..)
, defaultConfig

, forRequest
, forResponse
, request
, response
, rawRequest
, rawResponse

, ForkM (..)
, ServerM (..)
, ClientM (..)
, HttpM (..)
, HttpM'
, RawHttpM (..)
, RawHttpM'
, SockM (..)
, ClientAddressM (..)
, ServerAddressM (..)
, AddressM'
, QueueM (..)
, SendM (..)
, FlushM (..)
, BodyM (..)
, PayloadM (..)
, Contains (..)
, (&)
)
where

-- Middle ware.
import Network.Salvia.Core.Config
import Network.Salvia.Core.Aspects

