module Network.Salvia.Handler.Parser (hParser) where

import Control.Monad.State
import Network.Protocol.Http
import Network.Salvia.Httpd
import System.IO
import System.Timeout

{- |
The 'hParser' handler is used to parse the raw request message into the
'Message' data type. This handler is generally used as (one of) the first
handlers in an environment. The first handler argument is executed when the
request is invalid, possibly due to parser errors, and is parametrized with the
error string. The second handler argument is executed when the request is
valid. When the message could be parsed within the time specified with the
first argument the function silently returns.
-}

hParser
  :: (Socket m, Request m, MonadIO m)
  => Int               -- ^ Timeout in milliseconds.
  -> (String -> m a)   -- ^ The fail handler.
  -> m a
  -> m a
hParser t onfail onsuccess =
  do h <- sock
     -- TODO use try and fail with bad request or reject silently.
     mMsg <- liftIO $ timeout (t * 1000) $
       -- TODO: Using NoBuffering here may crash the entire program (GHC
       -- runtime?) when processing more requests than just a few:
       do hSetBuffering h (BlockBuffering (Just (64*1024)))
          fmap Just (readHeader h) `catch` const (return Nothing)
     case join mMsg of
       Nothing -> onfail "internal IO error"
       Just msg -> 
         do case parseRequest (msg "") of
              Left err -> onfail (show err)
              Right x -> do
                request (put x)
                onsuccess

-- Read all lines until the first empty line.
readHeader :: Handle -> IO (String -> String)
readHeader h =
  do l <- hGetLine h
     let lf = showChar '\n'
     if l `elem` ["", "\r"]
       then return lf
       else liftM ((showString l . lf) .) (readHeader h)

