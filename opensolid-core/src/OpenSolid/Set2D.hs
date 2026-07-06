module OpenSolid.Set2D
  ( Set2D
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

import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Prelude
import OpenSolid.Set (Set)
import OpenSolid.Set qualified as Set

type Set2D units item = Set (Bounds2D units) item

pattern Leaf :: Bounds2D units -> item -> Set2D units item
pattern Leaf leafBounds leafItem <- Set.Leaf{leafBounds, leafItem}

pattern Node :: Bounds2D units -> NonEmpty (Set2D units item) -> Set2D units item
pattern Node nodeBounds children <- Set.Node{nodeBounds, children}

{-# COMPLETE Node, Leaf #-}

bounds :: Set2D units item -> Bounds2D units
bounds = Set.bounds

leaf :: Bounds2D units -> item -> Set2D units item
leaf = Set.leaf

node :: NonEmpty (Set2D units item) -> Set2D units item
node = Set.node

size :: Set2D units item -> Int
size = Set.size

build :: (item -> Bounds2D units) -> NonEmpty item -> Set2D units item
build = Set.build

linear :: (item -> Bounds2D units) -> NonEmpty item -> Set2D units item
linear = Set.linear

aggregate :: NonEmpty (Set2D units item) -> Set2D units item
aggregate = Set.aggregate

flatten :: Set2D units (Set2D units item) -> Set2D units item
flatten = Set.flatten

fromList :: (item -> Bounds2D units) -> List item -> Maybe (Set2D units item)
fromList = Set.fromList

toNonEmpty :: Set2D units item -> NonEmpty item
toNonEmpty = Set.toNonEmpty

toNonEmptyOf :: (item -> a) -> Set2D units item -> NonEmpty a
toNonEmptyOf = Set.toNonEmptyOf

toNonEmptyWithIndex :: (Int -> item -> a) -> Set2D units item -> NonEmpty a
toNonEmptyWithIndex = Set.toNonEmptyWithIndex

toList :: Set2D units item -> List item
toList = Set.toList

toListOf :: (item -> a) -> Set2D units item -> List a
toListOf = Set.toListOf

toListWithIndex :: (Int -> item -> a) -> Set2D units item -> List a
toListWithIndex = Set.toListWithIndex

map :: (item1 -> item2) -> (item2 -> Bounds2D units2) -> Set2D units1 item1 -> Set2D units2 item2
map = Set.map

mapWithIndex ::
  (Int -> item1 -> item2) ->
  (item2 -> Bounds2D units2) ->
  Set2D units1 item1 ->
  Set2D units2 item2
mapWithIndex = Set.mapWithIndex

reverseMap ::
  (item1 -> item2) ->
  (item2 -> Bounds2D units2) ->
  Set2D units1 item1 ->
  Set2D units2 item2
reverseMap = Set.reverseMap

combine :: (item1 -> Set2D units2 item2) -> Set2D units1 item1 -> Set2D units2 item2
combine = Set.combine

combineWithIndex :: (Int -> item1 -> Set2D units2 item2) -> Set2D units1 item1 -> Set2D units2 item2
combineWithIndex = Set.combineWithIndex

cull :: (Bounds2D units -> Bool) -> Set2D units item -> List item
cull = Set.cull

filter :: (Bounds2D units -> Bool) -> (item -> Bool) -> Set2D units item -> List item
filter = Set.filter

filterMap :: (Bounds2D units -> Bool) -> (item -> Maybe a) -> Set2D units item -> List a
filterMap = Set.filterMap

filterWithIndex ::
  (Bounds2D units -> Bool) ->
  (Int -> item -> Bool) ->
  Set2D units item ->
  List item
filterWithIndex = Set.filterWithIndex

filterMapWithIndex ::
  (Bounds2D units -> Bool) ->
  (Int -> item -> Maybe a) ->
  Set2D units item ->
  List a
filterMapWithIndex = Set.filterMapWithIndex

subset :: (Bounds2D units -> Bool) -> (item -> Bool) -> Set2D units item -> Maybe (Set2D units item)
subset = Set.subset

any :: (Bounds2D units -> Bool) -> (item -> Bool) -> Set2D units item -> Bool
any = Set.any

all :: (Bounds2D units -> Bool) -> (item -> Bool) -> Set2D units item -> Bool
all = Set.all

pairwiseFilter ::
  (Bounds2D units1 -> Bounds2D units2 -> Bool) ->
  (item1 -> item2 -> Bool) ->
  Set2D units1 item1 ->
  Set2D units2 item2 ->
  List (item1, item2)
pairwiseFilter = Set.pairwiseFilter

pairwiseFilterMap ::
  (Bounds2D units1 -> Bounds2D units2 -> Bool) ->
  (item1 -> item2 -> Maybe a) ->
  Set2D units1 item1 ->
  Set2D units2 item2 ->
  List a
pairwiseFilterMap = Set.pairwiseFilterMap

pairwiseFilterWithIndices ::
  (Bounds2D units1 -> Bounds2D units2 -> Bool) ->
  (Int -> Int -> item1 -> item2 -> Bool) ->
  Set2D units1 item1 ->
  Set2D units2 item2 ->
  List (item1, item2)
pairwiseFilterWithIndices = Set.pairwiseFilterWithIndices

pairwiseFilterMapWithIndices ::
  (Bounds2D units1 -> Bounds2D units2 -> Bool) ->
  (Int -> Int -> item1 -> item2 -> Maybe a) ->
  Set2D units1 item1 ->
  Set2D units2 item2 ->
  List a
pairwiseFilterMapWithIndices = Set.pairwiseFilterMapWithIndices

clusters ::
  (Bounds2D units -> Bounds2D units -> Bool) ->
  (item -> item -> Bool) ->
  Set2D units item ->
  NonEmpty (NonEmpty item)
clusters = Set.clusters
