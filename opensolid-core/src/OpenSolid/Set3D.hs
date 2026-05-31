module OpenSolid.Set3D
  ( Set3D
  , pattern Leaf
  , pattern Node
  , singleton
  , size
  , bounds
  , build
  , linear
  , aggregate
  , flatten
  , toNonEmpty
  , toNonEmptyOf
  , toNonEmptyWithIndex
  , toList
  , toListOf
  , toListWithIndex
  , map
  , mapWithIndex
  , reverseMap
  , combine
  , combineWithIndex
  , union
  , cull
  , filter
  , filterMap
  , filterWithIndex
  , filterMapWithIndex
  , subset
  , any
  , all
  , forEach
  , forEachWithIndex
  , reverseForEach
  , reverseForEachWithIndex
  , pairwiseFilter
  , pairwiseFilterMap
  , pairwiseFilterWithIndices
  , pairwiseFilterMapWithIndices
  , clusters
  )
where

import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Chainable (Chainable)
import OpenSolid.Prelude
import OpenSolid.Set (Set)
import OpenSolid.Set qualified as Set

type Set3D space item = Set (Bounds3D space) item

pattern Leaf :: Bounds3D space -> item -> Set3D space item
pattern Leaf leafBounds leafItem <- Set.Leaf{leafBounds, leafItem}

pattern Node :: Bounds3D space -> Set3D space item -> Set3D space item -> Set3D space item
pattern Node nodeBounds leftChild rightChild <- Set.Node{nodeBounds, leftChild, rightChild}

{-# COMPLETE Node, Leaf #-}

size :: Set3D space item -> Int
size = Set.size

bounds :: Set3D space item -> Bounds3D space
bounds = Set.bounds

singleton :: Bounds3D space -> item -> Set3D space item
singleton = Set.singleton

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

toNonEmptyOf :: (item -> a) -> Set3D space item -> NonEmpty a
toNonEmptyOf = Set.toNonEmptyOf

toNonEmptyWithIndex :: (Int -> item -> a) -> Set3D space item -> NonEmpty a
toNonEmptyWithIndex = Set.toNonEmptyWithIndex

toList :: Set3D space item -> List item
toList = Set.toList

toListOf :: (item -> a) -> Set3D space item -> List a
toListOf = Set.toListOf

toListWithIndex :: (Int -> item -> a) -> Set3D space item -> List a
toListWithIndex = Set.toListWithIndex

map :: (item1 -> item2) -> (item2 -> Bounds3D space2) -> Set3D space1 item1 -> Set3D space2 item2
map = Set.map

mapWithIndex ::
  (Int -> item1 -> item2) ->
  (item2 -> Bounds3D space2) ->
  Set3D space1 item1 ->
  Set3D space2 item2
mapWithIndex = Set.mapWithIndex

reverseMap ::
  (item1 -> item2) ->
  (item2 -> Bounds3D space2) ->
  Set3D space1 item1 ->
  Set3D space2 item2
reverseMap = Set.reverseMap

combine :: (item1 -> Set3D space2 item2) -> Set3D space1 item1 -> Set3D space2 item2
combine = Set.combine

combineWithIndex :: (Int -> item1 -> Set3D space2 item2) -> Set3D space1 item1 -> Set3D space2 item2
combineWithIndex = Set.combineWithIndex

union :: Set3D space item -> Set3D space item -> Set3D space item
union = Set.union

cull :: (Bounds3D space -> Bool) -> Set3D space item -> List item
cull = Set.cull

filter :: (Bounds3D space -> Bool) -> (item -> Bool) -> Set3D space item -> List item
filter = Set.filter

filterMap :: (Bounds3D space -> Bool) -> (item -> Maybe a) -> Set3D space item -> List a
filterMap = Set.filterMap

filterWithIndex ::
  (Bounds3D space -> Bool) ->
  (Int -> item -> Bool) ->
  Set3D space item ->
  List item
filterWithIndex = Set.filterWithIndex

filterMapWithIndex ::
  (Bounds3D space -> Bool) ->
  (Int -> item -> Maybe a) ->
  Set3D space item ->
  List a
filterMapWithIndex = Set.filterMapWithIndex

subset :: (Bounds3D space -> Bool) -> (item -> Bool) -> Set3D space item -> Maybe (Set3D space item)
subset = Set.subset

any :: (Bounds3D space -> Bool) -> (item -> Bool) -> Set3D space item -> Bool
any = Set.any

all :: (Bounds3D space -> Bool) -> (item -> Bool) -> Set3D space item -> Bool
all = Set.all

forEach :: Chainable action => Set3D space item -> (item -> action) -> action
forEach = Set.forEach

forEachWithIndex :: Chainable action => Set3D space item -> (Int -> item -> action) -> action
forEachWithIndex = Set.forEachWithIndex

reverseForEach :: Chainable action => Set3D space item -> (item -> action) -> action
reverseForEach = Set.reverseForEach

reverseForEachWithIndex ::
  Chainable action =>
  Set3D space item ->
  (Int -> item -> action) ->
  action
reverseForEachWithIndex = Set.reverseForEachWithIndex

pairwiseFilter ::
  (Bounds3D space1 -> Bounds3D space2 -> Bool) ->
  (item1 -> item2 -> Bool) ->
  Set3D space1 item1 ->
  Set3D space2 item2 ->
  List (item1, item2)
pairwiseFilter = Set.pairwiseFilter

pairwiseFilterMap ::
  (Bounds3D space1 -> Bounds3D space2 -> Bool) ->
  (item1 -> item2 -> Maybe a) ->
  Set3D space1 item1 ->
  Set3D space2 item2 ->
  List a
pairwiseFilterMap = Set.pairwiseFilterMap

pairwiseFilterWithIndices ::
  (Bounds3D space1 -> Bounds3D space2 -> Bool) ->
  (Int -> Int -> item1 -> item2 -> Bool) ->
  Set3D space1 item1 ->
  Set3D space2 item2 ->
  List (item1, item2)
pairwiseFilterWithIndices = Set.pairwiseFilterWithIndices

pairwiseFilterMapWithIndices ::
  (Bounds3D space1 -> Bounds3D space2 -> Bool) ->
  (Int -> Int -> item1 -> item2 -> Maybe a) ->
  Set3D space1 item1 ->
  Set3D space2 item2 ->
  List a
pairwiseFilterMapWithIndices = Set.pairwiseFilterMapWithIndices

clusters ::
  (Bounds3D space -> Bounds3D space -> Bool) ->
  (item -> item -> Bool) ->
  Set3D space item ->
  NonEmpty (Set3D space item)
clusters = Set.clusters
