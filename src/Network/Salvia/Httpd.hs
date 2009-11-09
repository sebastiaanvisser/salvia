module Network.Salvia.Httpd
( Config (..)
, defaultConfig

, forRequest
, forResponse
, request
, response

, ServerM (..)
, ClientM (..)
, HttpM (..)
, HttpM'
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

