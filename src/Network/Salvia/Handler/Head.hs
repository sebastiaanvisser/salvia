{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.Head (hHead) where

import Control.Monad.Trans
import Control.Applicative hiding (empty)
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Handler.Rewrite
import Network.Salvia.Handler.Close
import Network.Salvia.Core.Aspects

{- |
The 'hHead' handler makes sure no `HTTP' `Response' body is sent to the client
when the request is an HTTP 'HEAD' request. In the case of a 'HEAD' request the
specified sub handler will be executed under the assumption that the request
was a 'GET' request, otherwise this handler will act as the identify function.
-}

hHead :: (MonadIO m, QueueM m, HttpM Request m) => m a -> m a
hHead handler =
  do m <- request (getM method)
     case m of
       HEAD -> hLocalRequest method (const GET) $
                 handler <* emptyQueue
       _    -> handler

