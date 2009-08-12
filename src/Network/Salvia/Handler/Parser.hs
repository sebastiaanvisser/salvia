module Network.Salvia.Handler.Parser {- doc ok -}
  ( hRequestParser
  , hResponseParser
  , hParser
  , readNonEmptyLines
  ) where

import Control.Applicative
import Control.Monad.State
import Network.Protocol.Http
import Network.Salvia.Core.Aspects
import System.IO
import System.Timeout
import Text.Parsec.Error (ParseError)

-- | Like the `hParser' but always parses `HTTP` `Requests`s.

hRequestParser
  :: (SocketM m, RequestM m, MonadIO m)
  => Int               -- ^ Timeout in milliseconds.
  -> (String -> m a)   -- ^ The fail handler.
  -> m a               -- ^ The success handler.
  -> m (Maybe a)
hRequestParser = hParser (request . put) parseRequest

-- | Like the `hParser' but always parses `HTTP` `Response`s.

hResponseParser
  :: (SocketM m, ResponseM m, MonadIO m)
  => Int               -- ^ Timeout in milliseconds.
  -> (String -> m a)   -- ^ The fail handler.
  -> m a               -- ^ The success handler.
  -> m (Maybe a)
hResponseParser = hParser (response . put) parseResponse

{- |
The 'hParser' handler is used to parse the raw `HTTP` message into the
'Message' data type. This handler is generally used as (one of) the first
handlers in a client or server environment. The first handler argument is
executed when the message is invalid, possibly due to parser errors, and is
parametrized with the error string. The second handler argument is executed
when the message is valid. When the message could not be parsed within the time
specified with the first argument the function silently returns.
-}

hParser
  :: (SocketM m, MonadIO m)
  => (HTTP d -> m b)                        -- ^ What to do with message.
  -> (String -> Either ParseError (HTTP d)) -- ^ Custom message parser.
  -> Int                                    -- ^ Timeout in milliseconds.
  -> (String -> m a)                        -- ^ The fail handler.
  -> m a                                    -- ^ The success handler.
  -> m (Maybe a)
hParser action p t onfail onsuccess =
  do h <- sock
     -- TODO use try and fail with bad request or reject silently.
     mMsg <- liftIO $ timeout (t * 1000) $
       -- TODO: Using NoBuffering here may crash the entire program (GHC
       -- runtime?) when processing more requests than just a few:
       do hSetBuffering h (BlockBuffering (Just (64*1024)))
          fmap Just (readNonEmptyLines h) `catch` const (return Nothing)
     case join mMsg of
       Nothing -> return Nothing
       Just msg -> 
         case p (msg "") of
            Left err -> Just <$> (onfail (show err))
            Right x  -> Just <$> (action x >> onsuccess)

-- Read all lines until the first empty line.
readNonEmptyLines :: Handle -> IO (String -> String)
readNonEmptyLines h =
  do l <- hGetLine h
     let lf = showChar '\n'
     if l `elem` ["", "\r"]
       then return lf
       else ((showString l . lf) .) <$> readNonEmptyLines h

