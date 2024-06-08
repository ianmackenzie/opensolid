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
import Prelude qualified

data Queue a = Queue Int (Deque a) deriving (Eq, Show)

instance Addition (Queue a) a (Queue a) where
  queue + item = push item queue

empty :: Queue a
empty = Queue 0 (Deque.fromConsAndSnocLists [] [])

singleton :: a -> Queue a
singleton value = Queue 1 (Deque.fromConsAndSnocLists [value] [])

isEmpty :: Queue a -> Bool
isEmpty (Queue n _) = n == 0

length :: Queue a -> Int
length (Queue n _) = n

push :: a -> Queue a -> Queue a
push value (Queue n deque) = Queue (n + 1) (Deque.snoc value deque)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue n deque) = Maybe.do
  (first, rest) <- Deque.uncons deque
  Just (first, Queue (n - 1) rest)

map :: (a -> b) -> Queue a -> Queue b
map function (Queue n deque) = Queue n (Prelude.fmap function deque)
