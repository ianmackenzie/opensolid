module OpenSolid.Set3D
  ( Set3D
  , pattern Leaf
  , pattern Node
  , one
  , two
  , size
  , bounds
  , build
  , linear
  , aggregate
  , toNonEmpty
  , toList
  , map
  , reverseMap
  , union
  , any
  , all
  , find
  , findWithIndex
  , findAll
  , findAllWithIndices
  , foldrMap
  , foldrMapWithIndex
  , foldlMap
  , foldlMapWithIndex
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

one :: (item, Bounds3D space) -> Set3D space item
one = Set.one

two :: (item, Bounds3D space) -> (item, Bounds3D space) -> Set3D space item
two = Set.two

build :: (item -> Bounds3D space) -> NonEmpty item -> Set3D space item
build = Set.build

linear :: (item -> Bounds3D space) -> NonEmpty item -> Set3D space item
linear = Set.linear

aggregate :: NonEmpty (Set3D space item) -> Set3D space item
aggregate = Set.aggregate

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

any :: (Bounds3D space -> Bool) -> (item -> Bool) -> Set3D space item -> Bool
any = Set.any

all :: (Bounds3D space -> Bool) -> (item -> Bool) -> Set3D space item -> Bool
all = Set.all

find :: Tolerance Meters => Bounds3D space -> Set3D space item -> Fuzzy (Maybe item)
find = Set.find

findWithIndex :: Tolerance Meters => Bounds3D space -> Set3D space item -> Fuzzy (Maybe (Int, item))
findWithIndex = Set.findWithIndex

findAll :: Tolerance Meters => Bounds3D space -> Set3D space item -> List item
findAll = Set.findAll

findAllWithIndices :: Tolerance Meters => Bounds3D space -> Set3D space item -> List (Int, item)
findAllWithIndices = Set.findAllWithIndices

foldrMap ::
  (item -> accumulated) ->
  (item -> accumulated -> accumulated) ->
  Set3D space item ->
  accumulated
foldrMap = Set.foldrMap

foldrMapWithIndex ::
  (Int -> item -> accumulated) ->
  (Int -> item -> accumulated -> accumulated) ->
  Set3D space item ->
  accumulated
foldrMapWithIndex = Set.foldrMapWithIndex

foldlMap ::
  (item -> accumulated) ->
  (accumulated -> item -> accumulated) ->
  Set3D space item ->
  accumulated
foldlMap = Set.foldlMap

foldlMapWithIndex ::
  (Int -> item -> accumulated) ->
  (Int -> accumulated -> item -> accumulated) ->
  Set3D space item ->
  accumulated
foldlMapWithIndex = Set.foldlMapWithIndex
