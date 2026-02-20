module OpenSolid.Queue
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

import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude

data Queue a = Queue Int (List a) (List a) deriving (Eq, Show)

instance Addition (Queue a) a (Queue a) where
  queue + item = push item queue

instance Addition (Queue a) (List a) (Queue a) where
  queue + items = List.foldl (+) queue items

empty :: Queue a
empty = Queue 0 [] []

singleton :: a -> Queue a
singleton value = Queue 1 [value] []

isEmpty :: Queue a -> Bool
isEmpty queue = length queue == 0

length :: Queue a -> Int
length (Queue n _ _) = n

push :: a -> Queue a -> Queue a
push value (Queue _ [] []) = singleton value
push value (Queue n head tail) = Queue (n + 1) head (value : tail)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue n (first : rest) tail) = Just (first, Queue (n - 1) rest tail)
pop (Queue n [] (NonEmpty tail)) = do
  let (first :| rest) = NonEmpty.reverse tail
  Just (first, Queue (n - 1) rest [])
pop (Queue _ [] []) = Nothing

map :: (a -> b) -> Queue a -> Queue b
map function (Queue n head tail) = Queue n (List.map function head) (List.map function tail)
