module Network.Salvia.Httpd (

  -- Httpd.Core.Config
    Config (..)
  , defaultConfig

  -- Httpd.Core.Aspects

  , ServerConfig
  , config

  , Request
  , request

  , Response
  , response

  , Socket
  , rawSock
  , sock
  , raw
  , peer

  , Send
  , sendStr
  , sendStrLn
  , sendBs
  , spoolStr
  , spoolBs
  , flushQueue
  , flushRequest
  , flushResponse
  , emptyQueue

  , reset


  , Contents
  , contents


  -- Httpd.Core.Handler
{-  , Context
  , config
  , request
  , response
  , sock
  , address
  , queue-}

{-  , mkContext-}

{-  , Handler
  , ResourceHandler
  , UriHandler-}

  -- Httpd.Core.IO
{-  , send
  , sendStr
  , sendStrLn
  , sendBs-}

{-  , spool
  , spoolBs-}

{-  , flushHeaders
  , flushQueue-}

{-  , emptyQueue
  , reset-}

{-  , contents
  , contentsUtf8
  , uriEncodedPostParamsUTF8-}

  -- Httpd.Core.Server
  , server

  -- Httpd.Core.Client
  , client
  , getRequest

  ) where

-- Abstract implementation.
import Network.Salvia.Core.Config
import Network.Salvia.Core.Aspects

-- Concrete implementation.
-- import Network.Salvia.Core.Context
-- import Network.Salvia.Core.Handler
import Network.Salvia.Core.Server
import Network.Salvia.Core.Client

