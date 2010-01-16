{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.Parser
  ( hRequestParser
  , hResponseParser
  , hParser
  , readNonEmptyLines
  )
where

import Control.Applicative
import Control.Monad.State hiding (sequence)
import Data.Traversable
import Network.Protocol.Http
import Network.Salvia.Core.Aspects
import Network.Salvia.Handler.Error
import Prelude hiding (sequence)
import System.IO
import System.Timeout

-- | Like the `hParser' but always parses `HTTP` `Requests`s.

hRequestParser
  :: (SockM m, RawHttpM Request m, HttpM Request m, MonadIO m)
  => Int               -- ^ Timeout in milliseconds.
  -> (String -> m a)   -- ^ The fail handler.
  -> m a               -- ^ The success handler.
  -> m (Maybe a)
hRequestParser = hParser pt parseRequest
  where pt x = do request (put x)
                  rawRequest (put x)

-- | Like the `hParser' but always parses `HTTP` `Response`s.

hResponseParser
  :: (SockM m, RawHttpM Response m, HttpM Response m, MonadIO m)
  => Int               -- ^ Timeout in milliseconds.
  -> (String -> m a)   -- ^ The fail handler.
  -> m a               -- ^ The success handler.
  -> m (Maybe a)
hResponseParser = hParser pt parseResponse
  where pt x = do response (put x)
                  rawResponse (put x)

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
  :: (SockM m, MonadIO m)
  => (Http d -> m b)                        -- ^ What to do with message.
  -> (String -> Either String (Http d))     -- ^ Custom message parser.
  -> Int                                    -- ^ Timeout in milliseconds.
  -> (String -> m a)                        -- ^ The fail handler.
  -> m a                                    -- ^ The success handler.
  -> m (Maybe a)
hParser action parse t onfail onsuccess =
  do h <- sock
     mmsg <-
       liftM join
         . flip catchIO Nothing
         . timeout (t * 1000)
         $ do hSetBuffering h (BlockBuffering (Just (64*1024)))
              Just <$> readNonEmptyLines h
     let hndl = (onfail . show) `either` (\x -> action x >> onsuccess)
     sequence (hndl . parse <$> mmsg)

-- Read all lines until the first empty line.
readNonEmptyLines :: Handle -> IO String
readNonEmptyLines h = ($"") <$> f
  where
    f = do l <- hGetLine h
           let lf = showChar '\n'
           if null l || l == "\r"
             then return lf
             else ((showString l . lf) .) <$> f

