module OpenSolid.Set3D
  ( Set3D
  , pattern Leaf
  , pattern Node
  , one
  , two
  , bounds
  , partition
  , partitionBy
  , linear
  , linearBy
  , toNonEmpty
  , toList
  , union
  , find
  , findWithIndex
  , findAll
  , findAllWithIndices
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

bounds :: Set3D space item -> Bounds3D space
bounds = Set.bounds

one :: (item, Bounds3D space) -> Set3D space item
one = Set.one

two :: (item, Bounds3D space) -> (item, Bounds3D space) -> Set3D space item
two = Set.two

partition :: NonEmpty (item, Bounds3D space) -> Set3D space item
partition = Set.partition

partitionBy :: (item -> Bounds3D space) -> NonEmpty item -> Set3D space item
partitionBy = Set.partitionBy

linear :: NonEmpty (item, Bounds3D space) -> Set3D space item
linear = Set.linear

linearBy :: (item -> Bounds3D space) -> NonEmpty item -> Set3D space item
linearBy = Set.linearBy

toNonEmpty :: Set3D space item -> NonEmpty item
toNonEmpty = Set.toNonEmpty

toList :: Set3D space item -> List item
toList = Set.toList

union :: Set3D space item -> Set3D space item -> Set3D space item
union = Set.union

find :: Tolerance Meters => Bounds3D space -> Set3D space item -> Fuzzy (Maybe item)
find = Set.find

findWithIndex :: Tolerance Meters => Bounds3D space -> Set3D space item -> Fuzzy (Maybe (Int, item))
findWithIndex = Set.findWithIndex

findAll :: Tolerance Meters => Bounds3D space -> Set3D space item -> List item
findAll = Set.findAll

findAllWithIndices :: Tolerance Meters => Bounds3D space -> Set3D space item -> List (Int, item)
findAllWithIndices = Set.findAllWithIndices
