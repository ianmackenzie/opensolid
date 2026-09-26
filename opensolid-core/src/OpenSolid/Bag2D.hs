module OpenSolid.Bag2D
  ( Bag2D
  , data Empty
  , data Full
  , empty
  , full
  , toMaybeSet
  , fromMaybeSet
  , isEmpty
  , size
  , singleton
  , group
  , pack
  , aggregate
  , toList
  , toListOf
  , flatten
  , map
  , combine
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
  , pairwiseAny
  , pairwiseAnyBounds
  , pairwiseAnyItems
  , clusters
  , uniqueItems
  )
where

import OpenSolid.Bag (Bag)
import OpenSolid.Bag qualified as Bag
import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Prelude
import OpenSolid.Set2D (Set2D)

type Bag2D units item = Bag (Bounds2D units) item

pattern Empty :: Bag2D units item
pattern Empty <- Bag.Empty

pattern Full :: Set2D units item -> Bag2D units item
pattern Full set <- Bag.Full set

{-# COMPLETE Empty, Full #-}

empty :: Bag2D units item
empty = Bag.empty

full :: Set2D units item -> Bag2D units item
full = Bag.full

toMaybeSet :: Bag2D units item -> Maybe (Set2D units item)
toMaybeSet = Bag.toMaybeSet

fromMaybeSet :: Maybe (Set2D units item) -> Bag2D units item
fromMaybeSet = Bag.fromMaybeSet

isEmpty :: Bag2D units item -> Bool
isEmpty = Bag.isEmpty

size :: Bag2D units item -> Int
size = Bag.size

singleton :: Bounded item (Bounds2D units) => item -> Bag2D units item
singleton = Bag.singleton

group :: List (Bag2D units item) -> Bag2D units item
group = Bag.group

pack :: Bounded item (Bounds2D units) => List item -> Bag2D units item
pack = Bag.pack

aggregate :: List (Bag2D units item) -> Bag2D units item
aggregate = Bag.aggregate

toList :: Bag2D units item -> List item
toList = Bag.toList

toListOf :: (item1 -> item2) -> Bag2D units item1 -> List item2
toListOf = Bag.toListOf

flatten :: Bag2D units (Bag2D units item) -> Bag2D units item
flatten = Bag.flatten

map ::
  Bounded item2 (Bounds2D units2) =>
  (item1 -> item2) ->
  Bag2D units1 item1 ->
  Bag2D units2 item2
map = Bag.map

combine :: (item1 -> Bag2D units2 item2) -> Bag2D units1 item1 -> Bag2D units2 item2
combine = Bag.combine

filter :: (Bounds2D units -> Bool) -> (item -> Bool) -> Bag2D units item -> Bag2D units item
filter = Bag.filter

filterBounds :: (Bounds2D units -> Bool) -> Bag2D units item -> Bag2D units item
filterBounds = Bag.filterBounds

filterItems :: (item -> Bool) -> Bag2D units item -> Bag2D units item
filterItems = Bag.filterItems

filterMap ::
  Bounded item2 (Bounds2D units2) =>
  (Bounds2D units1 -> Bool) ->
  (item1 -> Maybe item2) ->
  Bag2D units1 item1 ->
  Bag2D units2 item2
filterMap = Bag.filterMap

filterMapItems ::
  Bounded item2 (Bounds2D units2) =>
  (item1 -> Maybe item2) ->
  Bag2D units1 item1 ->
  Bag2D units2 item2
filterMapItems = Bag.filterMapItems

any :: (Bounds2D units -> Bool) -> (item -> Bool) -> Bag2D units item -> Bool
any = Bag.any

anyBounds :: (Bounds2D units -> Bool) -> Bag2D units item -> Bool
anyBounds = Bag.anyBounds

anyItem :: (item -> Bool) -> Bag2D units item -> Bool
anyItem = Bag.anyItem

all :: (Bounds2D units -> Bool) -> (item -> Bool) -> Bag2D units item -> Bool
all = Bag.all

allBounds :: (Bounds2D units -> Bool) -> Bag2D units item -> Bool
allBounds = Bag.allBounds

allItems :: (item -> Bool) -> Bag2D units item -> Bool
allItems = Bag.allItems

pairwiseAny ::
  (Bounds2D units1 -> Bounds2D units2 -> Bool) ->
  (item1 -> item2 -> Bool) ->
  Bag2D units1 item1 ->
  Bag2D units2 item2 ->
  Bool
pairwiseAny = Bag.pairwiseAny

pairwiseAnyBounds ::
  (Bounds2D units1 -> Bounds2D units2 -> Bool) ->
  Bag2D units1 item1 ->
  Bag2D units2 item2 ->
  Bool
pairwiseAnyBounds = Bag.pairwiseAnyBounds

pairwiseAnyItems ::
  (item1 -> item2 -> Bool) ->
  Bag2D units1 item1 ->
  Bag2D units2 item2 ->
  Bool
pairwiseAnyItems = Bag.pairwiseAnyItems

clusters ::
  (Bounds2D units -> Bounds2D units -> Bool) ->
  (item -> item -> Bool) ->
  Bag2D units item ->
  List (NonEmpty item)
clusters = Bag.clusters

uniqueItems ::
  ( ApproximateEquality item units
  , Bounded item (Bounds2D units)
  , Tolerance units
  ) =>
  Bag2D units item ->
  List item
uniqueItems = Bag.uniqueItems
