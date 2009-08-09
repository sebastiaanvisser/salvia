{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Core.Aspects where

import Control.Applicative
import Control.Monad.State
import Network.Protocol.Http
import System.IO
import qualified Network.Salvia.Core.Config as Server
import qualified Data.ByteString.Lazy as B
import qualified Network.Socket as S

class (Applicative m, Monad m) => ServerConfig m where
  config :: m Server.Config





class (Applicative m, Monad m) => Request m where
  request :: State Message a -> m a

class (Applicative m, Monad m) => Response m where
  response :: State Message a -> m a



-- Access to the raw socket to the other end point.

class (Applicative m, Monad m) => Socket m where
  rawSock :: m S.Socket
  sock    :: m Handle
  raw     :: ((S.Socket, Handle) -> IO ()) -> m ()
  peer    :: m S.SockAddr




class (Applicative m, Monad m) => Send m where
  sendStr       :: String                                   -> m ()
  sendBs        :: B.ByteString                             -> m ()
  spoolStr      :: (String       -> String)       -> Handle -> m ()
  spoolBs       :: (B.ByteString -> B.ByteString) -> Handle -> m ()
  flushRequest  :: m ()
  flushResponse :: m ()
  flushQueue    :: m ()
  emptyQueue    :: m ()

class (Applicative m, Monad m) => Contents m where
  contents :: m (Maybe B.ByteString)

{- | Reset both the send queue and the generated server response. -}

reset :: (Response m, Send m) => m ()
reset =
  do response (put emptyResponse)
     emptyQueue

sendStrLn :: Send m => String -> m ()
sendStrLn = sendStr . (++"\n")

