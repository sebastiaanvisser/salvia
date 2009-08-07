module Network.Salvia.Httpd (

  -- Httpd.Core.Config
    HttpdConfig (..)
  , defaultConfig

  -- Httpd.Core.Aspects

  , Config
  , config

  , Client
  , address

  , Request
  , request

  , Response
  , response

  , Socket
  , rawSock
  , sock

  , Send
  , sendStr
  , sendStrLn
  , sendBs
  , spoolStr
  , spoolBs
  , flushQueue
  , flushHeaders
  , emptyQueue

  , reset


  , Receive
  , contents
  , contentsUtf8
  , uriEncodedPostParamsUTF8


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

  -- Httpd.Core.Main
  , start

  ) where

-- Abstract implementation.
import Network.Salvia.Core.Config
import Network.Salvia.Core.Aspects

-- Concrete implementation.
-- import Network.Salvia.Core.Context
-- import Network.Salvia.Core.Handler
import Network.Salvia.Core.Main

