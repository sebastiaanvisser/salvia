module Network.Salvia.Handler.Error (
    hError
  , hCustomError
  , hIOError
  , hSafeIO
  ) where

import Control.Monad.Trans
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Core.Aspects
import System.IO.Error

{- |
The 'hError' handler enables the creation of a default style of error responses
for the specified HTTP `Status` code.
-}

hError :: (Response m, Send m) => Status -> m ()
hError e = hCustomError e
  (concat ["[", show (codeFromStatus e), "] ", show e, "\n"])

{- | Like `hError` but with a custom error message. -}

hCustomError :: (Response m, Send m) => Status -> String -> m ()
hCustomError e m = do
  response $
    do setM status e
       setM contentLength (Just $ length m)
       setM contentType ("text/plain", Nothing)
  sendStr m

{- |
Map an `IOError` to a default style error response.

The mapping from an IO error to an error response is rather straightforward:

>  | isDoesNotExistError e = hError NotFound
>  | isAlreadyInUseError e = hError ServiceUnavailable
>  | isPermissionError   e = hError Forbidden
>  | True                  = hError InternalServerError
-}

hIOError :: (Response m, Send m) => IOError -> m ()
hIOError e
  | isDoesNotExistError e = hError NotFound
  | isAlreadyInUseError e = hError ServiceUnavailable
  | isPermissionError   e = hError Forbidden
  | True                  = hError InternalServerError

{- |
Execute an handler with the result of an IO action. When the IO actions fails a
default error handler will be executed.
-}

hSafeIO
  :: (MonadIO m, Response m, Send m)
  => IO a -> (a -> m ()) -> m ()
hSafeIO io h = liftIO (try io) >>= either hIOError h
