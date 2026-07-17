module OpenSolid.Bag
  ( Bag (Empty, Full)
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

import OpenSolid.IndexOutOfBounds (IndexOutOfBounds (..))
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Set (Set)
import OpenSolid.Set qualified as Set
import OpenSolid.Units qualified as Units
import Prelude qualified

data Bag b a = Empty | Full (Set b a) deriving (Show)

instance Set.Bounds b => Prelude.Semigroup (Bag b a) where
  Empty <> bag = bag
  bag <> Empty = bag
  Full set1 <> Full set2 = Full (set1 <> set2)

instance
  (Units.Coercion b1 b2, Units.Coercion a1 a2) =>
  Units.Coercion (Bag b1 a1) (Bag b2 a2)
  where
  coerce Empty = Empty
  coerce (Full set) = Full (Units.coerce set)

instance Indexed (Bag b a) Int a where
  Empty !! index = throw IndexOutOfBounds{index, size = 0}
  Full set !! index = set !! index

empty :: Bag b a
empty = Empty

full :: Set b a -> Bag b a
full = Full

toMaybeSet :: Bag b a -> Maybe (Set b a)
toMaybeSet Empty = Nothing
toMaybeSet (Full set) = Just set

fromMaybeSet :: Maybe (Set b a) -> Bag b a
fromMaybeSet Nothing = Empty
fromMaybeSet (Just set) = Full set

isEmpty :: Bag b a -> Bool
isEmpty Empty = True
isEmpty Full{} = False

size :: Bag b a -> Int
size Empty = 0
size (Full set) = Set.size set

singleton :: b -> a -> Bag b a
singleton givenBounds givenItem = Full (Set.leaf givenBounds givenItem)

group :: Set.Bounds b => List (Bag b a) -> Bag b a
group bags =
  case List.filterMap toMaybeSet bags of
    [] -> Empty
    NonEmpty subsets -> Full (Set.node subsets)

pack :: Set.Bounds b => (a -> b) -> List a -> Bag b a
pack _ [] = Empty
pack boundsFunction (NonEmpty items) = Full (Set.build boundsFunction items)

aggregate :: Set.Bounds b => List (Bag b a) -> Bag b a
aggregate bags =
  case List.filterMap toMaybeSet bags of
    [] -> Empty
    NonEmpty sets -> Full (Set.aggregate sets)

toList :: Bag b a -> List a
toList = toListOf id

toListOf :: (a1 -> a2) -> Bag b a1 -> List a2
toListOf _ Empty = []
toListOf function (Full set) = Set.toListOf function set

map :: Set.Bounds b2 => (a1 -> a2) -> (a2 -> b2) -> Bag b1 a1 -> Bag b2 a2
map _ _ Empty = Empty
map function boundsFunction (Full set) = Full (Set.map function boundsFunction set)

cull :: (b -> Bool) -> Bag b a -> List a
cull _ Empty = []
cull boundsFunction (Full set) = Set.cull boundsFunction set

any :: (b -> Bool) -> (a -> Bool) -> Bag b a -> Bool
any _ _ Empty = False
any boundsPredicate itemPredicate (Full set) =
  Set.any boundsPredicate itemPredicate set

all :: (b -> Bool) -> (a -> Bool) -> Bag b a -> Bool
all _ _ Empty = True
all boundsPredicate itemPredicate (Full set) =
  Set.all boundsPredicate itemPredicate set

pairwiseAny :: (b1 -> b2 -> Bool) -> (a1 -> a2 -> Bool) -> Bag b1 a1 -> Bag b2 a2 -> Bool
pairwiseAny _ _ Empty _ = False
pairwiseAny _ _ _ Empty = False
pairwiseAny boundsPredicate itemPredicate (Full set1) (Full set2) =
  Set.pairwiseAny boundsPredicate itemPredicate set1 set2

clusters :: Set.Bounds b => (b -> b -> Bool) -> (a -> a -> Bool) -> Bag b a -> List (NonEmpty a)
clusters _ _ Empty = []
clusters boundsPredicate itemPredicate (Full set) =
  NonEmpty.toList (Set.clusters boundsPredicate itemPredicate set)
