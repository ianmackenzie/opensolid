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
  , toList
  , map
  , reverseMap
  , union
  , cull
  , cullIndexed
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

toList :: Set3D space item -> List item
toList = Set.toList

map :: (a -> b) -> (b -> Bounds3D space2) -> Set3D space1 a -> Set3D space2 b
map = Set.map

reverseMap :: (a -> b) -> (b -> Bounds3D space2) -> Set3D space1 a -> Set3D space2 b
reverseMap = Set.reverseMap

union :: Set3D space item -> Set3D space item -> Set3D space item
union = Set.union

cull :: (Bounds3D space -> Bool) -> Set3D space item -> List item
cull = Set.cull

cullIndexed :: (Bounds3D space -> Bool) -> Set3D space item -> List (Int, item)
cullIndexed = Set.cullIndexed
