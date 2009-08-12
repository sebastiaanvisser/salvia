{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Core.Aspects where

import Control.Applicative
import Control.Monad.State
import Network.Protocol.Http
import Network.Salvia.Core.Config
import Network.Socket
import System.IO
import qualified Data.ByteString.Lazy as B

class (Applicative m, Monad m) => ConfigM m where
  config :: m Config

class (Applicative m, Monad m) => RequestM m where
  request :: State (HTTP Request) a -> m a

class (Applicative m, Monad m) => ResponseM m where
  response :: State (HTTP Response) a -> m a

-- Access to the raw socket to the other end point.

class (Applicative m, Monad m) => SocketM m where
  rawSock :: m Socket
  sock    :: m Handle
  raw     :: ((Socket, Handle) -> IO ()) -> m ()
  peer    :: m SockAddr

class (Applicative m, Monad m) => SendM m where
  sendStr       :: String                                   -> m ()
  sendBs        :: B.ByteString                             -> m ()
  spoolStr      :: (String       -> String)       -> Handle -> m ()
  spoolBs       :: (B.ByteString -> B.ByteString) -> Handle -> m ()
  flushRequest  :: m ()
  flushResponse :: m ()
  flushQueue    :: m ()
  emptyQueue    :: m ()

class (Applicative m, Monad m) => ContentsM m where
  contents :: m (Maybe B.ByteString)

{- | Reset both the send queue and the generated server response. -}

reset :: (ResponseM m, SendM m) => m ()
reset =
  do response (put emptyResponse)
     emptyQueue

sendStrLn :: SendM m => String -> m ()
sendStrLn = sendStr . (++"\n")

