module Network.Salvia.Handler.Dispatching
  ( Dispatcher
  , ListDispatcher
  , hDispatch
  , hListDispatch

  , hRequestDispatch
  , hResponseDispatch
  )
where

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
A list dispatcher takes a mapping from dispatch values to handlers and one
default fallback handler.
-}

type ListDispatcher a m b = [(a, m b)] -> m b -> m b

{- |
Dispatch on an arbitrary part of the context using an arbitrary predicate. When
the predicate returns true on the value selected with the `Label` the first
handler will be invoked, otherwise the second handler will be used.
-}

hDispatch
  :: forall a b c d m. HttpM d m => d -> (Http d :-> b) -> (c -> b -> Bool) -> c -> m a -> m a -> m a
hDispatch _ f match a handler _default =
  do let h = http :: State (Http d) b -> m b
     ctx <- h (getM f)
     if a `match` ctx
       then handler
       else _default

{- |
Turns a dispatcher function into a list dispatcher. This enables handler
routing based on arbitrary values from the context. When non of the predicates
in the `ListDispatcher` type hold the default handler will be invoked.
-}

hListDispatch :: Dispatcher a m b -> ListDispatcher a m b
hListDispatch disp = flip $ foldr $ uncurry disp

{- |
Like the `hDispatch` but always dispatches on a (part of) the `HTTP
Request' part of the context.
-}

hRequestDispatch :: HttpM Request m => (Http Request :-> b) -> (t -> b -> Bool) -> Dispatcher t m c
hRequestDispatch = hDispatch forRequest

{- |
Like the `hDispatch` but always dispatches on a (part of) the `HTTP
Response' part of the context.
-}

hResponseDispatch :: HttpM Response m => (Http Response :-> b) -> (t -> b -> Bool) -> Dispatcher t m c
hResponseDispatch = hDispatch forResponse

