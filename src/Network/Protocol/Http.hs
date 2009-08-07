module Network.Protocol.Http(

  -- * HTTP message data types.

    Method (..)
  , methods
  , Version (Version)
  , HeaderKey
  , HeaderValue
  , Headers
  , Direction (Request, Response)
  , Message (Message)

  -- * Creating (parts of) messages.

  , emptyRequest
  , emptyResponse
  , http10
  , http11

  -- * Accessing fields.

  , major
  , minor
  , body
  , headers
  , version
  , direction
  , method
  , uri
  , status

  -- * Accessing specific header fields.

  , normalizeHeader
  , header

  , utf8

  , connection
  , contentLength
  , contentType
  , cookie
  , date
  , hostname
  , keepAlive
  , location
  , server

  -- * Parsing HTTP messages.

  , parseRequest
  , parseResponse

  -- * Printing HTTP messages.

  , showMessageHeader

  -- * Handling HTTP status codes.

  , Status (..)
  , statusCodes
  , statusFailure
  , statusFromCode
  , codeFromStatus

  ) where

import Network.Protocol.Http.Data
import Network.Protocol.Http.Parser
import Network.Protocol.Http.Printer
import Network.Protocol.Http.Status

