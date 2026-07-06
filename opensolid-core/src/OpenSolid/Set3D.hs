module OpenSolid.Set3D
  ( Set3D
  , pattern Leaf
  , pattern Node
  , leaf
  , node
  , size
  , bounds
  , build
  , linear
  , aggregate
  , flatten
  , fromList
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
  , cull
  , filter
  , filterMap
  , filterWithIndex
  , filterMapWithIndex
  , subset
  , any
  , all
  , pairwiseFilter
  , pairwiseFilterMap
  , pairwiseFilterWithIndices
  , pairwiseFilterMapWithIndices
  , clusters
  )
where

import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Prelude
import OpenSolid.Set (Set)
import OpenSolid.Set qualified as Set

type Set3D space item = Set (Bounds3D space) item

pattern Leaf :: Bounds3D space -> item -> Set3D space item
pattern Leaf leafBounds leafItem <- Set.Leaf{leafBounds, leafItem}

pattern Node :: Bounds3D space -> NonEmpty (Set3D space item) -> Set3D space item
pattern Node nodeBounds children <- Set.Node{nodeBounds, children}

{-# COMPLETE Node, Leaf #-}

size :: Set3D space item -> Int
size = Set.size

bounds :: Set3D space item -> Bounds3D space
bounds = Set.bounds

leaf :: Bounds3D space -> item -> Set3D space item
leaf = Set.leaf

node :: NonEmpty (Set3D space item) -> Set3D space item
node = Set.node

build :: (item -> Bounds3D space) -> NonEmpty item -> Set3D space item
build = Set.build

linear :: (item -> Bounds3D space) -> NonEmpty item -> Set3D space item
linear = Set.linear

aggregate :: NonEmpty (Set3D space item) -> Set3D space item
aggregate = Set.aggregate

flatten :: Set3D space (Set3D space item) -> Set3D space item
flatten = Set.flatten

fromList :: (item -> Bounds3D space) -> List item -> Maybe (Set3D space item)
fromList = Set.fromList

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
  NonEmpty (NonEmpty item)
clusters = Set.clusters
