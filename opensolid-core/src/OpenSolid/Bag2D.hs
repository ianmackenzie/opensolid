module OpenSolid.Bag2D
  ( Bag2D
  , pattern Empty
  , pattern Full
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
  , map
  , cull
  , any
  , all
  , pairwiseAny
  , clusters
  )
where

import OpenSolid.Bag (Bag)
import OpenSolid.Bag qualified as Bag
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

singleton :: Bounds2D units -> item -> Bag2D units item
singleton = Bag.singleton

group :: List (Bag2D units item) -> Bag2D units item
group = Bag.group

pack :: (item -> Bounds2D units) -> List item -> Bag2D units item
pack = Bag.pack

aggregate :: List (Bag2D units item) -> Bag2D units item
aggregate = Bag.aggregate

toList :: Bag2D units item -> List item
toList = Bag.toList

toListOf :: (item1 -> item2) -> Bag2D units item1 -> List item2
toListOf = Bag.toListOf

map :: (item1 -> item2) -> (item2 -> Bounds2D units2) -> Bag2D units1 item1 -> Bag2D units2 item2
map = Bag.map

cull :: (Bounds2D units -> Bool) -> Bag2D units item -> List item
cull = Bag.cull

any :: (Bounds2D units -> Bool) -> (item -> Bool) -> Bag2D units item -> Bool
any = Bag.any

all :: (Bounds2D units -> Bool) -> (item -> Bool) -> Bag2D units item -> Bool
all = Bag.all

pairwiseAny ::
  (Bounds2D units1 -> Bounds2D units2 -> Bool) ->
  (item1 -> item2 -> Bool) ->
  Bag2D units1 item1 ->
  Bag2D units2 item2 ->
  Bool
pairwiseAny = Bag.pairwiseAny

clusters ::
  (Bounds2D units -> Bounds2D units -> Bool) ->
  (item -> item -> Bool) ->
  Bag2D units item ->
  List (NonEmpty item)
clusters = Bag.clusters
