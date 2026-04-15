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
  , toList
  , map
  , reverseMap
  , union
  , cull
  , cullIndexed
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

toList :: Set2D units item -> List item
toList = Set.toList

map :: (a -> b) -> (b -> Bounds2D units2) -> Set2D units1 a -> Set2D units2 b
map = Set.map

reverseMap :: (a -> b) -> (b -> Bounds2D units2) -> Set2D units1 a -> Set2D units2 b
reverseMap = Set.reverseMap

union :: Set2D units item -> Set2D units item -> Set2D units item
union = Set.union

cull :: (Bounds2D units -> Bool) -> Set2D units item -> List item
cull = Set.cull

cullIndexed :: (Bounds2D units -> Bool) -> Set2D units item -> List (Int, item)
cullIndexed = Set.cullIndexed
