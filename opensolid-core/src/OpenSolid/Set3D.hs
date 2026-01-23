module OpenSolid.Set3D
  ( Set3D
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

import OpenSolid.Bounded3D (Bounded3D)
import OpenSolid.Bounded3D qualified as Bounded3D
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Fuzzy (Fuzzy)
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
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

one :: Bounded3D item space => item -> Set3D space item
one item = Set.one (item, Bounded3D.bounds item)

two :: Bounded3D item space => item -> item -> Set3D space item
two firstItem secondItem =
  Set.two (firstItem, Bounded3D.bounds firstItem) (secondItem, Bounded3D.bounds secondItem)

fromNonEmpty :: Bounded3D item space => NonEmpty item -> Set3D space item
fromNonEmpty givenItems =
  Set.fromNonEmpty (NonEmpty.map (Pair.decorate Bounded3D.bounds) givenItems)

toNonEmpty :: Set3D space item -> NonEmpty item
toNonEmpty = Set.toNonEmpty

toList :: Set3D space item -> List item
toList = Set.toList

union :: Set3D space item -> Set3D space item -> Set3D space item
union = Set.union

find :: Tolerance Meters => Bounds3D space -> Set3D space item -> Fuzzy (Maybe item)
find = Set.find

filter :: Tolerance Meters => Bounds3D space -> Set3D space item -> List item
filter = Set.filter
