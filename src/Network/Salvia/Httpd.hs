module Network.Salvia.Httpd

  -- Httpd.Core.Config
  ( Config (..)
  , defaultConfig

  -- Httpd.Core.Aspects
  , forRequest
  , forResponse

  , ConfigM
  , config

  , HttpM
  , http
  , request
  , response

  , SocketM
  , rawSock
  , sock
  , peer

  , SendM
  , enqueue
  , dequeue
  , sendStr
  , sendBs
  , spoolStr
  , spoolBs

  , FlushM
  , flushHeaders
  , flushQueue

  , BodyM 
  , body

  -- Httpd.Core.Server
  , server

  -- Httpd.Core.Client
--   , client
--   , getRequest

  )
where

-- Abstract implementation.
import Network.Salvia.Core.Config
import Network.Salvia.Core.Aspects

-- Concrete implementation.
import Network.Salvia.Core.Server
-- import Network.Salvia.Core.Client

