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
  , findWithIndex
  , findAll
  , findAllWithIndices
  )
where

import Data.Void (Void)
import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Prelude
import OpenSolid.Set (Set)
import OpenSolid.Set qualified as Set

type Set2D units item = Set 2 units Void item

pattern Leaf :: Bounds2D units -> item -> Set2D units item
pattern Leaf leafBounds item = Set.Leaf leafBounds item

pattern Node :: Bounds2D units -> Set2D units item -> Set2D units item -> Set2D units item
pattern Node nodeBounds leftChild rightChild = Set.Node nodeBounds leftChild rightChild

{-# COMPLETE Node, Leaf #-}

bounds :: Set2D units item -> Bounds2D units
bounds = Set.bounds

one :: (item, Bounds2D units) -> Set2D units item
one = Set.one

two :: (item, Bounds2D units) -> (item, Bounds2D units) -> Set2D units item
two = Set.two

partition :: NonEmpty (item, Bounds2D units) -> Set2D units item
partition = Set.partition

partitionBy :: (item -> Bounds2D units) -> NonEmpty item -> Set2D units item
partitionBy = Set.partitionBy

toNonEmpty :: Set2D units item -> NonEmpty item
toNonEmpty = Set.toNonEmpty

toList :: Set2D units item -> List item
toList = Set.toList

union :: Set2D units item -> Set2D units item -> Set2D units item
union = Set.union

find :: Tolerance units => Bounds2D units -> Set2D units item -> Fuzzy (Maybe item)
find = Set.find

findWithIndex :: Tolerance units => Bounds2D units -> Set2D units item -> Fuzzy (Maybe (Int, item))
findWithIndex = Set.findWithIndex

findAll :: Tolerance units => Bounds2D units -> Set2D units item -> List item
findAll = Set.findAll

findAllWithIndices :: Tolerance units => Bounds2D units -> Set2D units item -> List (Int, item)
findAllWithIndices = Set.findAllWithIndices
