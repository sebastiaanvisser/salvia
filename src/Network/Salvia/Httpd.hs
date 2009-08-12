module Network.Salvia.Httpd (

  -- Httpd.Core.Config
    Config (..)
  , defaultConfig

  -- Httpd.Core.Aspects
  , ConfigM
  , config

  , RequestM
  , request

  , ResponseM
  , response

  , SocketM
  , rawSock
  , sock
  , raw
  , peer

  , SendM
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

  , ContentsM
  , contents

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
import Network.Salvia.Core.Server
import Network.Salvia.Core.Client

