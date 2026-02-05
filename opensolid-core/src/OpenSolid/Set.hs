module OpenSolid.Set
  ( Set (Node, Leaf)
  , bounds
  , one
  , two
  , partition
  , partitionBy
  , toNonEmpty
  , toList
  , union
  , find
  , filter
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.InternalError (InternalError (InternalError))
import OpenSolid.Interval qualified as Interval
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude

data Set dimension units space item where
  Leaf ::
    Bounds dimension units space ->
    item ->
    Set dimension units space item
  Node ::
    Bounds dimension units space ->
    Set dimension units space item ->
    Set dimension units space item ->
    Set dimension units space item

bounds ::
  Bounds.Exists dimension units space =>
  Set dimension units space item ->
  Bounds dimension units space
bounds (Node nodeBounds _ _) = nodeBounds
bounds (Leaf leafBounds _) = leafBounds

one ::
  Bounds.Exists dimension units space =>
  (item, Bounds dimension units space) ->
  Set dimension units space item
one (item, itemBounds) = Leaf itemBounds item

two ::
  Bounds.Exists dimension units space =>
  (item, Bounds dimension units space) ->
  (item, Bounds dimension units space) ->
  Set dimension units space item
two (firstItem, firstBounds) (secondItem, secondBounds) = do
  let nodeBounds = Bounds.aggregate2 firstBounds secondBounds
  Node nodeBounds (Leaf firstBounds firstItem) (Leaf secondBounds secondItem)

partition ::
  Bounds.Exists dimension units space =>
  NonEmpty (item, Bounds dimension units space) ->
  Set dimension units space item
partition boundedItems = build (NonEmpty.length boundedItems) boundedItems 0

partitionBy ::
  Bounds.Exists dimension units space =>
  (item -> Bounds dimension units space) ->
  NonEmpty item ->
  Set dimension units space item
partitionBy function items = partition (NonEmpty.map (Pair.decorate function) items)

build ::
  Bounds.Exists dimension units space =>
  Int ->
  NonEmpty (item, Bounds dimension units space) ->
  Int ->
  Set dimension units space item
build count boundedItems index
  | count == 1 = assert (NonEmpty.length boundedItems == 1) do
      let (item, itemBounds) = NonEmpty.first boundedItems
      Leaf itemBounds item
  | otherwise = assert (count >= 2 && NonEmpty.length boundedItems == count) do
      let indexedCoordinateMidpoint (_, itemBounds) =
            Interval.midpoint (Bounds.cyclicCoordinate index itemBounds)
      let sorted = NonEmpty.sortBy indexedCoordinateMidpoint boundedItems
      let leftCount = count // 2
      let rightCount = count - leftCount
      let (leftBoundedItems, rightBoundedItems) = splitAtIndex leftCount sorted
      let leftChild = build leftCount leftBoundedItems (index + 1)
      let rightChild = build rightCount rightBoundedItems (index + 1)
      let nodeBounds = Bounds.aggregate2 (bounds leftChild) (bounds rightChild)
      Node nodeBounds leftChild rightChild

splitAtIndex :: Int -> NonEmpty a -> (NonEmpty a, NonEmpty a)
splitAtIndex 0 _ = throw (InternalError "Bad split index in Set.build")
splitAtIndex _ (_ :| []) = throw (InternalError "Bad split index in Set.build")
splitAtIndex 1 (first :| NonEmpty rest) = (NonEmpty.one first, rest)
splitAtIndex n (first :| NonEmpty rest) =
  Pair.mapFirst (NonEmpty.push first) (splitAtIndex (n - 1) rest)

toNonEmpty :: Set dimension units space item -> NonEmpty item
toNonEmpty (Node _ leftChild rightChild) = gather leftChild (toNonEmpty rightChild)
toNonEmpty (Leaf _ item) = NonEmpty.one item

toList :: Set dimension units space item -> List item
toList = NonEmpty.toList . toNonEmpty

gather :: Set dimension units space item -> NonEmpty item -> NonEmpty item
gather set accumulated = case set of
  Node _ leftChild rightChild -> gather leftChild (gather rightChild accumulated)
  Leaf _ item -> NonEmpty.push item accumulated

union ::
  Bounds.Exists dimension units space =>
  Set dimension units space item ->
  Set dimension units space item ->
  Set dimension units space item
union left right = do
  let aggregateBounds = Bounds.aggregate2 (bounds left) (bounds right)
  Node aggregateBounds left right

find ::
  (Bounds.Exists dimension units space, Tolerance units) =>
  Bounds dimension units space ->
  Set dimension units space item ->
  Fuzzy (Maybe item)
find searchBounds set = case set of
  Node nodeBounds leftChild rightChild
    | not (nodeBounds `intersects` searchBounds) -> Resolved Nothing -- No overlapping items
    | Bounds.contains nodeBounds searchBounds -> Unresolved -- More than one overlapping item
    | otherwise -> case (find searchBounds leftChild, find searchBounds rightChild) of
        (Unresolved, _) -> Unresolved -- More than one item found just in the left
        (_, Unresolved) -> Unresolved -- More than one item found just in the right
        (Resolved Nothing, rightResult) -> rightResult -- If nothing found in the left, use the right result
        (leftResult, Resolved Nothing) -> leftResult -- If nothing found in the right, use the left result
        (Resolved (Just _), Resolved (Just _)) -> Unresolved -- Found exactly one item in each side
  Leaf itemBounds item ->
    Resolved (if searchBounds `intersects` itemBounds then Just item else Nothing)

filter ::
  (Bounds.Exists dimension units space, Tolerance units) =>
  Bounds dimension units space ->
  Set dimension units space item ->
  List item
filter searchBounds set = search searchBounds set []

search ::
  (Bounds.Exists dimension units space, Tolerance units) =>
  Bounds dimension units space ->
  Set dimension units space item ->
  List item ->
  List item
search searchBounds set accumulated = case set of
  Node nodeBounds leftChild rightChild
    | not (nodeBounds `intersects` searchBounds) -> accumulated
    | Bounds.contains nodeBounds searchBounds -> returnAll set accumulated
    | otherwise -> search searchBounds leftChild (search searchBounds rightChild accumulated)
  Leaf itemBounds item ->
    if searchBounds `intersects` itemBounds then item : accumulated else accumulated

returnAll :: Set dimension units space item -> List item -> List item
returnAll set accumulated = case set of
  Node _ leftChild rightChild -> returnAll leftChild (returnAll rightChild accumulated)
  Leaf _ item -> item : accumulated
