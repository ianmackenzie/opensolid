module OpenSolid.Set2D
  ( Set2D
  , pattern Leaf
  , pattern Node
  , one
  , two
  , size
  , bounds
  , build
  , linear
  , aggregate
  , toNonEmpty
  , toList
  , map
  , reverseMap
  , union
  , any
  , anyWithIndex
  , all
  , allWithIndex
  , intersecting
  , filter
  , filterWithIndex
  , filterMap
  , filterMapWithIndex
  , partition
  , partitionWithIndex
  , foldrMap
  , foldrMapWithIndex
  , foldlMap
  , foldlMapWithIndex
  )
where

import Data.Void (Void)
import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Prelude
import OpenSolid.Set (Set)
import OpenSolid.Set qualified as Set

type Set2D units item = Set 2 units Void item

pattern Leaf :: Bounds2D units -> item -> Set2D units item
pattern Leaf leafBounds item = Set.Leaf leafBounds item

pattern Node :: Bounds2D units -> Set2D units item -> Set2D units item -> Set2D units item
pattern Node nodeBounds leftChild rightChild = Set.Node nodeBounds leftChild rightChild

{-# COMPLETE Node, Leaf #-}

bounds :: Set2D units item -> Bounds2D units
bounds = Set.bounds

one :: (item, Bounds2D units) -> Set2D units item
one = Set.one

two :: (item, Bounds2D units) -> (item, Bounds2D units) -> Set2D units item
two = Set.two

size :: Set2D units item -> Int
size = Set.size

build :: (item -> Bounds2D units) -> NonEmpty item -> Set2D units item
build = Set.build

linear :: (item -> Bounds2D units) -> NonEmpty item -> Set2D units item
linear = Set.linear

aggregate :: NonEmpty (Set2D units item) -> Set2D units item
aggregate = Set.aggregate

toNonEmpty :: Set2D units item -> NonEmpty item
toNonEmpty = Set.toNonEmpty

toList :: Set2D units item -> List item
toList = Set.toList

map :: (a -> b) -> (b -> Bounds2D units2) -> Set2D units1 a -> Set2D units2 b
map = Set.map

reverseMap :: (a -> b) -> (b -> Bounds2D units2) -> Set2D units1 a -> Set2D units2 b
reverseMap = Set.reverseMap

union :: Set2D units item -> Set2D units item -> Set2D units item
union = Set.union

any :: (Bounds2D units -> Bool) -> (item -> Bool) -> Set2D units item -> Bool
any = Set.any

anyWithIndex :: (Bounds2D units -> Bool) -> (Int -> item -> Bool) -> Set2D units item -> Bool
anyWithIndex = Set.anyWithIndex

all :: (Bounds2D units -> Bool) -> (item -> Bool) -> Set2D units item -> Bool
all = Set.all

allWithIndex :: (Bounds2D units -> Bool) -> (Int -> item -> Bool) -> Set2D units item -> Bool
allWithIndex = Set.allWithIndex

intersecting ::
  ( Intersects target (Bounds2D units) constraint1
  , Intersects target item constraint2
  , constraint1
  , constraint2
  ) =>
  target ->
  Set2D units item ->
  List item
intersecting = Set.intersecting

filter :: (Bounds2D units -> Bool) -> (item -> Bool) -> Set2D units item -> List item
filter = Set.filter

filterWithIndex ::
  (Bounds2D units -> Bool) ->
  (Int -> item -> Bool) ->
  Set2D units item ->
  List (Int, item)
filterWithIndex = Set.filterWithIndex

filterMap :: (Bounds2D units -> Bool) -> (item -> Maybe a) -> Set2D units item -> List a
filterMap = Set.filterMap

filterMapWithIndex ::
  (Bounds2D units -> Bool) ->
  (Int -> item -> Maybe a) ->
  Set2D units item ->
  List (Int, a)
filterMapWithIndex = Set.filterMapWithIndex

partition ::
  (Bounds2D units -> Fuzzy Bool) ->
  (item -> Bool) ->
  Set2D units item ->
  (List item, List item)
partition = Set.partition

partitionWithIndex ::
  (Bounds2D units -> Fuzzy Bool) ->
  (Int -> item -> Bool) ->
  Set2D units item ->
  (List (Int, item), List (Int, item))
partitionWithIndex = Set.partitionWithIndex

foldrMap ::
  (item -> accumulated) ->
  (item -> accumulated -> accumulated) ->
  Set2D units item ->
  accumulated
foldrMap = Set.foldrMap

foldrMapWithIndex ::
  (Int -> item -> accumulated) ->
  (Int -> item -> accumulated -> accumulated) ->
  Set2D units item ->
  accumulated
foldrMapWithIndex = Set.foldrMapWithIndex

foldlMap ::
  (item -> accumulated) ->
  (accumulated -> item -> accumulated) ->
  Set2D units item ->
  accumulated
foldlMap = Set.foldlMap

foldlMapWithIndex ::
  (Int -> item -> accumulated) ->
  (Int -> accumulated -> item -> accumulated) ->
  Set2D units item ->
  accumulated
foldlMapWithIndex = Set.foldlMapWithIndex
