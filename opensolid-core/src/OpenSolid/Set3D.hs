module OpenSolid.Set3D
  ( Set3D
  , data Leaf
  , data Node
  , leaf
  , node
  , size
  , bounds
  , build
  , linear
  , aggregate
  , flatten
  , extend
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
  , filter
  , filterBounds
  , filterItems
  , filterMap
  , filterMapItems
  , any
  , anyBounds
  , anyItem
  , all
  , allBounds
  , allItems
  , pairwiseFilter
  , pairwiseFilterBounds
  , pairwiseFilterItems
  , pairwiseFilterMap
  , pairwiseFilterMapItems
  , pairwiseFilterWithIndices
  , pairwiseFilterMapWithIndices
  , pairwiseAny
  , pairwiseAnyBounds
  , pairwiseAnyItems
  , clusters
  )
where

import {-# SOURCE #-} OpenSolid.Bag3D (Bag3D)
import OpenSolid.Bounded (Bounded)
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

leaf :: Bounded item (Bounds3D space) => item -> Set3D space item
leaf = Set.leaf

node :: NonEmpty (Set3D space item) -> Set3D space item
node = Set.node

build :: Bounded item (Bounds3D space) => NonEmpty item -> Set3D space item
build = Set.build

linear :: Bounded item (Bounds3D space) => NonEmpty item -> Set3D space item
linear = Set.linear

aggregate :: NonEmpty (Set3D space item) -> Set3D space item
aggregate = Set.aggregate

flatten :: Set3D space (Set3D space item) -> Set3D space item
flatten = Set.flatten

extend :: Set3D space item -> Bag3D space item -> Set3D space item
extend = Set.extend

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

map ::
  Bounded item2 (Bounds3D space2) =>
  (item1 -> item2) ->
  Set3D space1 item1 ->
  Set3D space2 item2
map = Set.map

mapWithIndex ::
  Bounded item2 (Bounds3D space2) =>
  (Int -> item1 -> item2) ->
  Set3D space1 item1 ->
  Set3D space2 item2
mapWithIndex = Set.mapWithIndex

reverseMap ::
  Bounded item2 (Bounds3D space2) =>
  (item1 -> item2) ->
  Set3D space1 item1 ->
  Set3D space2 item2
reverseMap = Set.reverseMap

combine :: (item1 -> Set3D space2 item2) -> Set3D space1 item1 -> Set3D space2 item2
combine = Set.combine

combineWithIndex :: (Int -> item1 -> Set3D space2 item2) -> Set3D space1 item1 -> Set3D space2 item2
combineWithIndex = Set.combineWithIndex

filter :: (Bounds3D space -> Bool) -> (item -> Bool) -> Set3D space item -> Bag3D space item
filter = Set.filter

filterBounds :: (Bounds3D space -> Bool) -> Set3D space item -> Bag3D space item
filterBounds = Set.filterBounds

filterItems :: (item -> Bool) -> Set3D space item -> Bag3D space item
filterItems = Set.filterItems

filterMap ::
  Bounded item2 (Bounds3D space2) =>
  (Bounds3D space1 -> Bool) ->
  (item1 -> Maybe item2) ->
  Set3D space1 item1 ->
  Bag3D space2 item2
filterMap = Set.filterMap

filterMapItems ::
  Bounded item2 (Bounds3D space2) =>
  (item1 -> Maybe item2) ->
  Set3D space1 item1 ->
  Bag3D space2 item2
filterMapItems = Set.filterMapItems

any :: (Bounds3D space -> Bool) -> (item -> Bool) -> Set3D space item -> Bool
any = Set.any

anyBounds :: (Bounds3D space -> Bool) -> Set3D space item -> Bool
anyBounds = Set.anyBounds

anyItem :: (item -> Bool) -> Set3D space item -> Bool
anyItem = Set.anyItem

all :: (Bounds3D space -> Bool) -> (item -> Bool) -> Set3D space item -> Bool
all = Set.all

allBounds :: (Bounds3D space -> Bool) -> Set3D space item -> Bool
allBounds = Set.allBounds

allItems :: (item -> Bool) -> Set3D space item -> Bool
allItems = Set.allItems

pairwiseFilter ::
  (Bounds3D space1 -> Bounds3D space2 -> Bool) ->
  (item1 -> item2 -> Bool) ->
  Set3D space1 item1 ->
  Set3D space2 item2 ->
  List (item1, item2)
pairwiseFilter = Set.pairwiseFilter

pairwiseFilterBounds ::
  (Bounds3D space1 -> Bounds3D space2 -> Bool) ->
  Set3D space1 item1 ->
  Set3D space2 item2 ->
  List (item1, item2)
pairwiseFilterBounds = Set.pairwiseFilterBounds

pairwiseFilterItems ::
  (item1 -> item2 -> Bool) ->
  Set3D space1 item1 ->
  Set3D space2 item2 ->
  List (item1, item2)
pairwiseFilterItems = Set.pairwiseFilterItems

pairwiseFilterMap ::
  (Bounds3D space1 -> Bounds3D space2 -> Bool) ->
  (item1 -> item2 -> Maybe a) ->
  Set3D space1 item1 ->
  Set3D space2 item2 ->
  List a
pairwiseFilterMap = Set.pairwiseFilterMap

pairwiseFilterMapItems ::
  (item1 -> item2 -> Maybe a) ->
  Set3D space1 item1 ->
  Set3D space2 item2 ->
  List a
pairwiseFilterMapItems = Set.pairwiseFilterMapItems

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

pairwiseAny ::
  (Bounds3D space1 -> Bounds3D space2 -> Bool) ->
  (item1 -> item2 -> Bool) ->
  Set3D space1 item1 ->
  Set3D space2 item2 ->
  Bool
pairwiseAny = Set.pairwiseAny

pairwiseAnyBounds ::
  (Bounds3D space1 -> Bounds3D space2 -> Bool) ->
  Set3D space1 item1 ->
  Set3D space2 item2 ->
  Bool
pairwiseAnyBounds = Set.pairwiseAnyBounds

pairwiseAnyItems ::
  (item1 -> item2 -> Bool) ->
  Set3D space1 item1 ->
  Set3D space2 item2 ->
  Bool
pairwiseAnyItems = Set.pairwiseAnyItems

clusters ::
  (Bounds3D space -> Bounds3D space -> Bool) ->
  (item -> item -> Bool) ->
  Set3D space item ->
  NonEmpty (NonEmpty item)
clusters = Set.clusters
