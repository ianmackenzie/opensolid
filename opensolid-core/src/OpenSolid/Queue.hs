module OpenSolid.Queue
  ( Queue
  , empty
  , singleton
  , fromList
  , fromNonEmpty
  , fromBag
  , fromSet
  , isEmpty
  , length
  , push
  , pop
  , map
  )
where

import OpenSolid.Bag (Bag)
import OpenSolid.Bag qualified as Bag
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Set (Set)
import OpenSolid.Set qualified as Set

data Queue a = Queue Int (List a) (List a) deriving (Show)

empty :: Queue a
empty = Queue 0 [] []

singleton :: a -> Queue a
singleton value = Queue 1 [value] []

fromList :: List a -> Queue a
fromList values = Queue (List.length values) values []

fromNonEmpty :: NonEmpty a -> Queue a
fromNonEmpty = fromList . NonEmpty.toList

fromBag :: Bag b a -> Queue a
fromBag = fromList . Bag.toList

fromSet :: Set b a -> Queue a
fromSet = fromList . Set.toList

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
