module Network.Protocol.Http {- doc ok -}

  -- * HTTP message data types.

  ( Method (..)
  , Version (..)
  , Key
  , Value
  , Headers (..)
  , Request (..)
  , Response (..)
  , Http (..)

  -- * Creating (parts of) messages.

  , http10
  , http11
  , emptyHeaders
  , emptyRequest
  , emptyResponse

  -- * Accessing fields.

  , methods
  , major
  , minor
  , headers
  , version
  , headline
  , method
  , uri
  , asUri
  , status
  , normalizeHeader
  , header

  -- * Accessing specific header fields.

  , contentLength
  , connection
  , keepAlive
  , cookie
  , setCookie
  , location
  , contentType
  , date
  , hostname
  , server
  , userAgent

  -- * Parsing HTTP messages.

  , parseRequest
  , parseResponse

  -- * Exposure of internal parsec parsers.
  , pRequest
  , pResponse
  , pHeaders
  , pVersion
  , pMethod

  -- * Handling HTTP status codes.

  , Status (..)
  , statusCodes
  , statusFailure
  , statusFromCode
  , codeFromStatus

  ) where

import Network.Protocol.Http.Data
import Network.Protocol.Http.Parser
import Network.Protocol.Http.Printer ()
import Network.Protocol.Http.Headers
import Network.Protocol.Http.Status

