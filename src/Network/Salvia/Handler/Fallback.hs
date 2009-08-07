module Network.Salvia.Handler.Fallback (hOr, hEither) where

import Control.Applicative
import Control.Monad.State
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Httpd

{- |
Until I figure out how to do this correctly, this handler somewow implements
the MonadPlus/Alternative instance using a custom function.

When the first handler fails the response will be reset and the second handler
is executed. Failure is indicated by an HTTP response `Status` bigger than or
equal to 400 (`BadRequest`). See `statusFailure`.
-}

hOr :: (Send m, Response m) => m a -> m a -> m a
hOr h0 h1 = do
  a  <- h0
  st <- response (getM status)
  if statusFailure st
    then reset >> h1
    else return a

{- |
Like the `hOr` function, but the types of the alternatives may differ because
the values are packed up in an `Either`.
-}

hEither :: (Send m, Response m) => m a -> m b -> m (Either a b)
hEither h0 h1 = (Left <$> h0) `hOr` (Right <$> h1)

