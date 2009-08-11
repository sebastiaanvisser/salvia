module Network.Salvia.Handler.Dispatching (
    Dispatcher
  , ListDispatcher
  , hGenericDispatch
  , hRequestDispatch
  , hResponseDispatch
  , hListDispatch
  ) where

import Control.Monad.State
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Core.Aspects

{- |
The dispatcher type takes one value to dispatch on and two handlers. The first
handler will be used when the predicate on the dispatch value returned `True`,
the second (default) handler will be used when the predicate returs
`False`.
-}

type Dispatcher a m b = a -> m b -> m b -> m b

{- |
A list dispatcher takes a mapping from dispatch values and handlers and one
default handler.
-}

type ListDispatcher a m b = [(a, m b)] -> m b -> m b

{- |
Dispatch on an arbitrary part of the context using an arbitrary predicate. When
the predicate returns true on the value selected with the `Label` the first
handler will be invoked, otherwise the second handler will be used.
-}

hGenericDispatch
  :: (MonadState s m, Monad n)
  => (m b -> n b) -> Label s b -> (t -> b -> Bool) -> t -> n c -> n c -> n c
hGenericDispatch g f match a handler _default = do
  ctx <- g (getM f)
  if a `match` ctx
    then handler
    else _default

hRequestDispatch :: Request m => Label Message b -> (t -> b -> Bool) -> Dispatcher t m c
hRequestDispatch = hGenericDispatch request

hResponseDispatch :: Response m => Label Message b -> (t -> b -> Bool) -> Dispatcher t m c
hResponseDispatch = hGenericDispatch response

{- |
Turns a dispatcher function into a list dispatcher. This enables handler
routing based on arbitrary values from the context. When non of the predicates
in the `ListDispatcher` type hold the default handler will be invoked.
-}

hListDispatch :: Dispatcher a m b -> ListDispatcher a m b
hListDispatch disp = flip $ foldr $ uncurry disp

