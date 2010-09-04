{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.Error
( hError
, hCustomError
, hIOError
, hSafeIO
, hCatchAnd500
, catchIO
)
where

import Control.Exception (SomeException)
import Control.Monad
import Control.Monad.Trans
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Interface
import System.IO.Error
import qualified Control.Monad.CatchIO as C

-- | The 'hError' handler enables the creation of a default style of error
-- responses for the specified HTTP `Status` code.

hError :: (HttpM Response m, SendM m) => Status -> m ()
hError e = hCustomError e
  (concat ["[", show (codeFromStatus e), "] ", show e, "\n"])

-- | Like `hError` but with a custom error message.

hCustomError :: (HttpM Response m, SendM m) => Status -> String -> m ()
hCustomError e m =
  do response $
       do status        =: e
          contentLength =: Just (length m)
          contentType   =: Just ("text/plain", Nothing)
     send m

{- |
Map an `IOError` to a default style error response.

The mapping from an IO error to an error response is rather straightforward:

>  | isDoesNotExistError e = hError NotFound
>  | isAlreadyInUseError e = hError ServiceUnavailable
>  | isPermissionError   e = hError Forbidden
>  | True                  = hError InternalServerError
-}

hIOError :: (HttpM Response m, SendM m) => IOError -> m ()
hIOError e
  | isDoesNotExistError e = hError NotFound
  | isAlreadyInUseError e = hError ServiceUnavailable
  | isPermissionError   e = hError Forbidden
  | otherwise             = hError InternalServerError

-- | Execute an handler with the result of an IO action. When the IO actions
-- fails a default error handler will be executed.

hSafeIO
  :: (MonadIO m, HttpM Response m, SendM m)
  => IO a -> (a -> m ()) -> m ()
hSafeIO io h = liftIO (try io) >>= either hIOError h

-- | Utility function to easily catch IO errors.

catchIO :: MonadIO m => IO a -> a -> m a
catchIO a b = liftIO (a `catch` (const (return b)))

-- | Catch all exceptions and build a 500 InternalServerError when this happens.

hCatchAnd500 :: (C.MonadCatchIO m, SendM m, HttpM Response m) => m () -> m ()
hCatchAnd500 = C.try >=> either (\e -> hCustomError InternalServerError (show (e :: SomeException))) return

