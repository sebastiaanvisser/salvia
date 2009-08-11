{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.Parser (
    hRequestParser
  , hResponseParser
  , hParser
  , readHeaders
  ) where

import Control.Monad.State
import Network.Protocol.Http
import Network.Salvia.Core.Aspects
import System.IO
import System.Timeout
import Text.Parsec.Error (ParseError)

{- |
The 'hParser' handler is used to parse the raw request message into the
'Message' data type. This handler is generally used as (one of) the first
handlers in an environment. The first handler argument is executed when the
request is invalid, possibly due to parser errors, and is parametrized with the
error string. The second handler argument is executed when the request is
valid. When the message could be parsed within the time specified with the
first argument the function silently returns.
-}

hRequestParser
  :: (Socket m, Request m, MonadIO m)
  => Int               -- ^ Timeout in milliseconds.
  -> (String -> m a)   -- ^ The fail handler.
  -> m a
  -> m (Maybe a)
hRequestParser = hParser (request . put) parseRequest

hResponseParser
  :: (Socket m, Response m, MonadIO m)
  => Int               -- ^ Timeout in milliseconds.
  -> (String -> m a)   -- ^ The fail handler.
  -> m a
  -> m (Maybe a)
hResponseParser = hParser (response . put) parseResponse

hParser
  :: (Socket m, MonadIO m)
  => (Message -> m b)
  -> (String -> Either ParseError Message)
  -> Int
  -> (String -> m a)
  -> m a
  -> m (Maybe a)
hParser action p t onfail onsuccess =
  do h <- sock
     -- TODO use try and fail with bad request or reject silently.
     mMsg <- liftIO $ timeout (t * 1000) $
       -- TODO: Using NoBuffering here may crash the entire program (GHC
       -- runtime?) when processing more requests than just a few:
       do hSetBuffering h (BlockBuffering (Just (64*1024)))
          fmap Just (readHeaders h) `catch` const (return Nothing)
     case join mMsg of
       Nothing -> return Nothing
       Just msg -> 
         case p (msg "") of
            Left err -> Just `liftM` (onfail (show err))
            Right x  -> Just `liftM` (action x >> onsuccess)

-- Read all lines until the first empty line.
readHeaders :: Handle -> IO (String -> String)
readHeaders h =
  do l <- hGetLine h
     let lf = showChar '\n'
     if l `elem` ["", "\r"]
       then return lf
       else liftM ((showString l . lf) .) (readHeaders h)

