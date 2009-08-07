module Network.Protocol.Http.Status where

import Data.Bimap (Bimap, fromList, lookup, lookupR)
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

{- | HTTP status codes. -}

data Status =
    Continue                     -- ^ 100
  | SwitchingProtocols           -- ^ 101
  | OK                           -- ^ 200
  | Created                      -- ^ 201
  | Accepted                     -- ^ 202
  | NonAuthoritativeInformation  -- ^ 203
  | NoContent                    -- ^ 204
  | ResetContent                 -- ^ 205
  | PartialContent               -- ^ 206
  | MultipleChoices              -- ^ 300
  | MovedPermanently             -- ^ 301
  | Found                        -- ^ 302
  | SeeOther                     -- ^ 303
  | NotModified                  -- ^ 304
  | UseProxy                     -- ^ 305
  | TemporaryRedirect            -- ^ 307
  | BadRequest                   -- ^ 400
  | Unauthorized                 -- ^ 401
  | PaymentRequired              -- ^ 402
  | Forbidden                    -- ^ 403
  | NotFound                     -- ^ 404
  | MethodNotAllowed             -- ^ 405
  | NotAcceptable                -- ^ 406
  | ProxyAuthenticationRequired  -- ^ 407
  | RequestTimeOut               -- ^ 408
  | Conflict                     -- ^ 409
  | Gone                         -- ^ 410
  | LengthRequired               -- ^ 411
  | PreconditionFailed           -- ^ 412
  | RequestEntityTooLarge        -- ^ 413
  | RequestURITooLarge           -- ^ 414
  | UnsupportedMediaType         -- ^ 415
  | RequestedRangeNotSatisfiable -- ^ 416
  | ExpectationFailed            -- ^ 417
  | InternalServerError          -- ^ 500
  | NotImplemented               -- ^ 501
  | BadGateway                   -- ^ 502
  | ServiceUnavailable           -- ^ 503
  | GatewayTimeOut               -- ^ 504
  | HTTPVersionNotSupported      -- ^ 505
  | CustomStatus Int
  deriving (Eq, Ord)

{- | rfc2616 sec6.1.1 Status Code and Reason Phrase. -}

instance Show Status where
  show Continue                     = "Continue"
  show SwitchingProtocols           = "Switching Protocols"
  show OK                           = "OK"
  show Created                      = "Created"
  show Accepted                     = "Accepted"
  show NonAuthoritativeInformation  = "Non-Authoritative Information"
  show NoContent                    = "No Content"
  show ResetContent                 = "Reset Content"
  show PartialContent               = "Partial Content"
  show MultipleChoices              = "Multiple Choices"
  show MovedPermanently             = "Moved Permanently"
  show Found                        = "Found"
  show SeeOther                     = "See Other"
  show NotModified                  = "Not Modified"
  show UseProxy                     = "Use Proxy"
  show TemporaryRedirect            = "Temporary Redirect"
  show BadRequest                   = "Bad Request"
  show Unauthorized                 = "Unauthorized"
  show PaymentRequired              = "Payment Required"
  show Forbidden                    = "Forbidden"
  show NotFound                     = "Not Found"
  show MethodNotAllowed             = "Method Not Allowed"
  show NotAcceptable                = "Not Acceptable"
  show ProxyAuthenticationRequired  = "Proxy Authentication Required"
  show RequestTimeOut               = "Request Time-out"
  show Conflict                     = "Conflict"
  show Gone                         = "Gone"
  show LengthRequired               = "Length Required"
  show PreconditionFailed           = "Precondition Failed"
  show RequestEntityTooLarge        = "Request Entity Too Large"
  show RequestURITooLarge           = "Request-URI Too Large"
  show UnsupportedMediaType         = "Unsupported Media Type"
  show RequestedRangeNotSatisfiable = "Requested range not satisfiable"
  show ExpectationFailed            = "Expectation Failed"
  show InternalServerError          = "Internal Server Error"
  show NotImplemented               = "Not Implemented"
  show BadGateway                   = "Bad Gateway"
  show ServiceUnavailable           = "Service Unavailable"
  show GatewayTimeOut               = "Gateway Time-out"
  show HTTPVersionNotSupported      = "HTTP Version not supported"
  show (CustomStatus _)             = "Unknown Status"

{- |
RFC2616 sec6.1.1 Status Code and Reason Phrase.

Bidirectional mapping from status numbers to codes.
-}

statusCodes :: Bimap Int Status
statusCodes = fromList [
    (100, Continue)
  , (101, SwitchingProtocols)
  , (200, OK)
  , (201, Created)
  , (202, Accepted)
  , (203, NonAuthoritativeInformation)
  , (204, NoContent)
  , (205, ResetContent)
  , (206, PartialContent)
  , (300, MultipleChoices)
  , (301, MovedPermanently)
  , (302, Found)
  , (303, SeeOther)
  , (304, NotModified)
  , (305, UseProxy)
  , (307, TemporaryRedirect)
  , (400, BadRequest)
  , (401, Unauthorized)
  , (402, PaymentRequired)
  , (403, Forbidden)
  , (404, NotFound)
  , (405, MethodNotAllowed)
  , (406, NotAcceptable)
  , (407, ProxyAuthenticationRequired)
  , (408, RequestTimeOut)
  , (409, Conflict)
  , (410, Gone)
  , (411, LengthRequired)
  , (412, PreconditionFailed)
  , (413, RequestEntityTooLarge)
  , (414, RequestURITooLarge)
  , (415, UnsupportedMediaType)
  , (416, RequestedRangeNotSatisfiable)
  , (417, ExpectationFailed)
  , (500, InternalServerError)
  , (501, NotImplemented)
  , (502, BadGateway)
  , (503, ServiceUnavailable)
  , (504, GatewayTimeOut)
  , (505, HTTPVersionNotSupported)
  ]

-- | Every status greater-than or equal to 400 is considered to be a failure.
statusFailure :: Status -> Bool
statusFailure st = codeFromStatus st >= 400

-- | Conversion from status numbers to codes.
statusFromCode :: Int -> Status
statusFromCode num =
    fromMaybe (CustomStatus num)
  $ lookup num statusCodes

-- | Conversion from status codes to numbers.
codeFromStatus :: Status -> Int
codeFromStatus st =
    fromMaybe 0 -- function is total, should not happen.
  $ lookupR st statusCodes

