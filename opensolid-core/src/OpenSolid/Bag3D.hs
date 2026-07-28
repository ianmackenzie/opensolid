module OpenSolid.Bag3D
  ( Bag3D
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
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Prelude
import OpenSolid.Set3D (Set3D)

type Bag3D space item = Bag (Bounds3D space) item

pattern Empty :: Bag3D space item
pattern Empty <- Bag.Empty

pattern Full :: Set3D space item -> Bag3D space item
pattern Full set <- Bag.Full set

{-# COMPLETE Empty, Full #-}

empty :: Bag3D space item
empty = Bag.empty

full :: Set3D space item -> Bag3D space item
full = Bag.full

toMaybeSet :: Bag3D space item -> Maybe (Set3D space item)
toMaybeSet = Bag.toMaybeSet

fromMaybeSet :: Maybe (Set3D space item) -> Bag3D space item
fromMaybeSet = Bag.fromMaybeSet

isEmpty :: Bag3D space item -> Bool
isEmpty = Bag.isEmpty

size :: Bag3D space item -> Int
size = Bag.size

singleton :: Bounds3D space -> item -> Bag3D space item
singleton = Bag.singleton

group :: List (Bag3D space item) -> Bag3D space item
group = Bag.group

pack :: (item -> Bounds3D space) -> List item -> Bag3D space item
pack = Bag.pack

aggregate :: List (Bag3D space item) -> Bag3D space item
aggregate = Bag.aggregate

toList :: Bag3D space item -> List item
toList = Bag.toList

toListOf :: (item1 -> item2) -> Bag3D space item1 -> List item2
toListOf = Bag.toListOf

map :: (item1 -> item2) -> (item2 -> Bounds3D space2) -> Bag3D space1 item1 -> Bag3D space2 item2
map = Bag.map

cull :: (Bounds3D space -> Bool) -> Bag3D space item -> Bag3D space item
cull = Bag.cull

any :: (Bounds3D space -> Bool) -> (item -> Bool) -> Bag3D space item -> Bool
any = Bag.any

all :: (Bounds3D space -> Bool) -> (item -> Bool) -> Bag3D space item -> Bool
all = Bag.all

pairwiseAny ::
  (Bounds3D space1 -> Bounds3D space2 -> Bool) ->
  (item1 -> item2 -> Bool) ->
  Bag3D space1 item1 ->
  Bag3D space2 item2 ->
  Bool
pairwiseAny = Bag.pairwiseAny

clusters ::
  (Bounds3D space -> Bounds3D space -> Bool) ->
  (item -> item -> Bool) ->
  Bag3D space item ->
  List (NonEmpty item)
clusters = Bag.clusters
