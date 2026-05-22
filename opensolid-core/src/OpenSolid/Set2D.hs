module OpenSolid.Set2D
  ( Set2D
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
  )
where

import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Chainable (Chainable)
import OpenSolid.Prelude
import OpenSolid.Set (Set)
import OpenSolid.Set qualified as Set

type Set2D units item = Set 2 units Void item

pattern Leaf :: Bounds2D units -> item -> Set2D units item
pattern Leaf leafBounds leafItem <- Set.Leaf{leafBounds, leafItem}

pattern Node :: Bounds2D units -> Set2D units item -> Set2D units item -> Set2D units item
pattern Node nodeBounds leftChild rightChild <- Set.Node{nodeBounds, leftChild, rightChild}

{-# COMPLETE Node, Leaf #-}

bounds :: Set2D units item -> Bounds2D units
bounds = Set.bounds

singleton :: Bounds2D units -> item -> Set2D units item
singleton = Set.singleton

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

union :: Set2D units item -> Set2D units item -> Set2D units item
union = Set.union

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

forEach :: Chainable action => Set2D units item -> (item -> action) -> action
forEach = Set.forEach

forEachWithIndex :: Chainable action => Set2D units item -> (Int -> item -> action) -> action
forEachWithIndex = Set.forEachWithIndex

reverseForEach :: Chainable action => Set2D units item -> (item -> action) -> action
reverseForEach = Set.reverseForEach

reverseForEachWithIndex ::
  Chainable action =>
  Set2D units item ->
  (Int -> item -> action) ->
  action
reverseForEachWithIndex = Set.reverseForEachWithIndex
