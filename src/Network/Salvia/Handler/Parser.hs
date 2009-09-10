module Network.Salvia.Handler.Parser {- doc ok -}
  ( hRequestParser
  , hResponseParser
  , hParser
  , readNonEmptyLines
  )
where

import Control.Applicative
import Control.Monad.State hiding (sequence)
import Data.Traversable
import Misc.Util
import Network.Protocol.Http
import Network.Salvia.Core.Aspects
import System.IO
import System.Timeout
import Text.Parsec.Error (ParseError)
import Prelude hiding (sequence)

-- | Like the `hParser' but always parses `HTTP` `Requests`s.

hRequestParser
  :: (PeerM m, HttpM Request m, MonadIO m)
  => Int               -- ^ Timeout in milliseconds.
  -> (String -> m a)   -- ^ The fail handler.
  -> m a               -- ^ The success handler.
  -> m (Maybe a)
hRequestParser = hParser (request . put) parseRequest

-- | Like the `hParser' but always parses `HTTP` `Response`s.

hResponseParser
  :: (PeerM m, HttpM Response m, MonadIO m)
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
  :: (PeerM m, MonadIO m)
  => (Http d -> m b)                        -- ^ What to do with message.
  -> (String -> Either ParseError (Http d)) -- ^ Custom message parser.
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

