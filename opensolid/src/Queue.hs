module Queue
  ( Queue
  , empty
  , singleton
  , isEmpty
  , length
  , push
  , pop
  , map
  )
where

import Deque.Strict (Deque)
import Deque.Strict qualified as Deque
import Maybe qualified
import OpenSolid
import Pair qualified
import Prelude qualified

newtype Queue a = Queue (Deque a) deriving (Eq, Show)

empty :: Queue a
empty = Queue (Deque.fromConsAndSnocLists [] [])

singleton :: a -> Queue a
singleton value = Queue (Deque.fromConsAndSnocLists [value] [])

isEmpty :: Queue a -> Bool
isEmpty (Queue deque) = Deque.null deque

length :: Queue a -> Int
length (Queue deque) = Prelude.length deque

push :: a -> Queue a -> Queue a
push value (Queue deque) = Queue (Deque.snoc value deque)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue deque) = Maybe.map (Pair.mapSecond Queue) (Deque.uncons deque)

map :: (a -> b) -> Queue a -> Queue b
map function (Queue deque) = Queue (Prelude.fmap function deque)
