module OpenSolid.Set2D
  ( Set2D
  , pattern Leaf
  , pattern Node
  , one
  , two
  , bounds
  , partition
  , partitionBy
  , toNonEmpty
  , toList
  , union
  , find
  , filter
  )
where

import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Fuzzy (Fuzzy)
import OpenSolid.Prelude
import OpenSolid.Set (Set)
import OpenSolid.Set qualified as Set

type Set2D units space item = Set 2 units space item

pattern Leaf :: Bounds2D units space -> item -> Set2D units space item
pattern Leaf leafBounds item = Set.Leaf leafBounds item

pattern Node ::
  Bounds2D units space ->
  Set2D units space item ->
  Set2D units space item ->
  Set2D units space item
pattern Node nodeBounds leftChild rightChild = Set.Node nodeBounds leftChild rightChild

{-# COMPLETE Node, Leaf #-}

bounds :: Set2D units space item -> Bounds2D units space
bounds = Set.bounds

one :: (item, Bounds2D units space) -> Set2D units space item
one = Set.one

two :: (item, Bounds2D units space) -> (item, Bounds2D units space) -> Set2D units space item
two = Set.two

partition :: NonEmpty (item, Bounds2D units space) -> Set2D units space item
partition = Set.partition

partitionBy :: (item -> Bounds2D units space) -> NonEmpty item -> Set2D units space item
partitionBy = Set.partitionBy

toNonEmpty :: Set2D units space item -> NonEmpty item
toNonEmpty = Set.toNonEmpty

toList :: Set2D units space item -> List item
toList = Set.toList

union :: Set2D units space item -> Set2D units space item -> Set2D units space item
union = Set.union

find :: Tolerance units => Bounds2D units space -> Set2D units space item -> Fuzzy (Maybe item)
find = Set.find

filter :: Tolerance units => Bounds2D units space -> Set2D units space item -> List item
filter = Set.filter
