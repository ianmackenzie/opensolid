module OpenSolid.Set3D
  ( Set3D
  , pattern Leaf
  , pattern Node
  , one
  , two
  , size
  , bounds
  , build
  , linear
  , aggregate
  , flatten
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

import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Prelude
import OpenSolid.Set (Set)
import OpenSolid.Set qualified as Set

type Set3D space item = Set 3 Meters space item

pattern Leaf :: Bounds3D space -> item -> Set3D space item
pattern Leaf leafBounds item = Set.Leaf leafBounds item

pattern Node :: Bounds3D space -> Set3D space item -> Set3D space item -> Set3D space item
pattern Node nodeBounds leftChild rightChild = Set.Node nodeBounds leftChild rightChild

{-# COMPLETE Node, Leaf #-}

size :: Set3D space item -> Int
size = Set.size

bounds :: Set3D space item -> Bounds3D space
bounds = Set.bounds

one :: (item, Bounds3D space) -> Set3D space item
one = Set.one

two :: (item, Bounds3D space) -> (item, Bounds3D space) -> Set3D space item
two = Set.two

build :: (item -> Bounds3D space) -> NonEmpty item -> Set3D space item
build = Set.build

linear :: (item -> Bounds3D space) -> NonEmpty item -> Set3D space item
linear = Set.linear

aggregate :: NonEmpty (Set3D space item) -> Set3D space item
aggregate = Set.aggregate

flatten :: Set3D space (Set3D space item) -> Set3D space item
flatten = Set.flatten

toNonEmpty :: Set3D space item -> NonEmpty item
toNonEmpty = Set.toNonEmpty

toList :: Set3D space item -> List item
toList = Set.toList

map :: (a -> b) -> (b -> Bounds3D space2) -> Set3D space1 a -> Set3D space2 b
map = Set.map

reverseMap :: (a -> b) -> (b -> Bounds3D space2) -> Set3D space1 a -> Set3D space2 b
reverseMap = Set.reverseMap

union :: Set3D space item -> Set3D space item -> Set3D space item
union = Set.union

any :: (Bounds3D space -> Bool) -> (item -> Bool) -> Set3D space item -> Bool
any = Set.any

anyWithIndex :: (Bounds3D space -> Bool) -> (Int -> item -> Bool) -> Set3D space item -> Bool
anyWithIndex = Set.anyWithIndex

all :: (Bounds3D space -> Bool) -> (item -> Bool) -> Set3D space item -> Bool
all = Set.all

allWithIndex :: (Bounds3D space -> Bool) -> (Int -> item -> Bool) -> Set3D space item -> Bool
allWithIndex = Set.allWithIndex

intersecting ::
  ( Intersects target (Bounds3D space) constraint1
  , Intersects target item constraint2
  , constraint1
  , constraint2
  ) =>
  target ->
  Set3D space item ->
  List item
intersecting = Set.intersecting

filter :: (Bounds3D space -> Bool) -> (item -> Bool) -> Set3D space item -> List item
filter = Set.filter

filterWithIndex ::
  (Bounds3D space -> Bool) ->
  (Int -> item -> Bool) ->
  Set3D space item ->
  List (Int, item)
filterWithIndex = Set.filterWithIndex

filterMap :: (Bounds3D space -> Bool) -> (item -> Maybe a) -> Set3D space item -> List a
filterMap = Set.filterMap

filterMapWithIndex ::
  (Bounds3D space -> Bool) ->
  (Int -> item -> Maybe a) ->
  Set3D space item ->
  List (Int, a)
filterMapWithIndex = Set.filterMapWithIndex

partition ::
  (Bounds3D space -> Fuzzy Bool) ->
  (item -> Bool) ->
  Set3D space item ->
  (List item, List item)
partition = Set.partition

partitionWithIndex ::
  (Bounds3D space -> Fuzzy Bool) ->
  (Int -> item -> Bool) ->
  Set3D space item ->
  (List (Int, item), List (Int, item))
partitionWithIndex = Set.partitionWithIndex

foldrMap ::
  (item -> accumulated) ->
  (item -> accumulated -> accumulated) ->
  Set3D space item ->
  accumulated
foldrMap = Set.foldrMap

foldrMapWithIndex ::
  (Int -> item -> accumulated) ->
  (Int -> item -> accumulated -> accumulated) ->
  Set3D space item ->
  accumulated
foldrMapWithIndex = Set.foldrMapWithIndex

foldlMap ::
  (item -> accumulated) ->
  (accumulated -> item -> accumulated) ->
  Set3D space item ->
  accumulated
foldlMap = Set.foldlMap

foldlMapWithIndex ::
  (Int -> item -> accumulated) ->
  (Int -> accumulated -> item -> accumulated) ->
  Set3D space item ->
  accumulated
foldlMapWithIndex = Set.foldlMapWithIndex
