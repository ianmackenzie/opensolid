module OpenSolid.Set2D
  ( Set2D
  , pattern Leaf
  , pattern Node
  , one
  , two
  , bounds
  , fromNonEmpty
  , toNonEmpty
  , toList
  , union
  , find
  , filter
  )
where

import OpenSolid.Bounded2D (Bounded2D)
import OpenSolid.Bounded2D qualified as Bounded2D
import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Fuzzy (Fuzzy)
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
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

one :: Bounded2D item units space => item -> Set2D units space item
one item = Set.one (item, Bounded2D.bounds item)

two :: Bounded2D item units space => item -> item -> Set2D units space item
two firstItem secondItem =
  Set.two (firstItem, Bounded2D.bounds firstItem) (secondItem, Bounded2D.bounds secondItem)

fromNonEmpty :: Bounded2D item units space => NonEmpty item -> Set2D units space item
fromNonEmpty givenItems =
  Set.fromNonEmpty (NonEmpty.map (Pair.decorate Bounded2D.bounds) givenItems)

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
