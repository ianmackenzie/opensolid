module OpenSolid.Set3D
  ( Set3D (Node, Leaf)
  , one
  , two
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
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.InternalError (InternalError (InternalError))
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude

data Set3D a space
  = Leaf (Bounds3D space) a
  | Node (Bounds3D space) (Set3D a space) (Set3D a space)

instance Bounded3D (Set3D a space) space where
  bounds = bounds

bounds :: Set3D a space -> Bounds3D space
bounds (Node nodeBounds _ _) = nodeBounds
bounds (Leaf leafBounds _) = leafBounds

one :: Bounded3D a space => a -> Set3D a space
one item = Leaf (Bounded3D.bounds item) item

two :: Bounded3D a space => a -> a -> Set3D a space
two firstItem secondItem = do
  let firstBounds = Bounded3D.bounds firstItem
  let secondBounds = Bounded3D.bounds secondItem
  let nodeBounds = Bounds3D.aggregate2 firstBounds secondBounds
  Node nodeBounds (Leaf firstBounds firstItem) (Leaf secondBounds secondItem)

fromNonEmpty :: Bounded3D a space => NonEmpty a -> Set3D a space
fromNonEmpty givenItems = do
  let boundedItem item = (Bounded3D.bounds item, item)
  let boundedItems = NonEmpty.map boundedItem givenItems
  buildRightward (NonEmpty.length boundedItems) boundedItems

buildRightward :: Int -> NonEmpty (Bounds3D space, a) -> Set3D a space
buildRightward = build Bounds3D.rightwardCoordinate buildForward

buildForward :: Int -> NonEmpty (Bounds3D space, a) -> Set3D a space
buildForward = build Bounds3D.forwardCoordinate buildUpward

buildUpward :: Int -> NonEmpty (Bounds3D space, a) -> Set3D a space
buildUpward = build Bounds3D.upwardCoordinate buildRightward

build ::
  (Bounds3D space -> Interval Meters) ->
  (Int -> NonEmpty (Bounds3D space, a) -> Set3D a space) ->
  Int ->
  NonEmpty (Bounds3D space, a) ->
  Set3D a space
build boundsCoordinate buildSubset n boundedItems
  | n == 1 = assert (NonEmpty.length boundedItems == 1) do
      let (itemBounds, item) = NonEmpty.first boundedItems
      Leaf itemBounds item
  | otherwise = assert (n >= 2 && NonEmpty.length boundedItems == n) do
      let sorted = NonEmpty.sortBy (Interval.midpoint . boundsCoordinate . Pair.first) boundedItems
      let leftN = n `div` 2
      let rightN = n - leftN
      let (leftBoundedItems, rightBoundedItems) = splitAtIndex leftN sorted
      let leftChild = buildSubset leftN leftBoundedItems
      let rightChild = buildSubset rightN rightBoundedItems
      let nodeBounds = Bounds3D.aggregate2 (bounds leftChild) (bounds rightChild)
      Node nodeBounds leftChild rightChild

splitAtIndex :: Int -> NonEmpty a -> (NonEmpty a, NonEmpty a)
splitAtIndex 0 _ = throw (InternalError "Bad split index in Set3D.new")
splitAtIndex _ (_ :| []) = throw (InternalError "Bad split index in Set3D.new")
splitAtIndex 1 (first :| NonEmpty rest) = (NonEmpty.one first, rest)
splitAtIndex n (first :| NonEmpty rest) =
  Pair.mapFirst (NonEmpty.push first) (splitAtIndex (n - 1) rest)

toNonEmpty :: Set3D a space -> NonEmpty a
toNonEmpty (Node _ leftChild rightChild) = gather leftChild (toNonEmpty rightChild)
toNonEmpty (Leaf _ item) = NonEmpty.one item

toList :: Set3D a space -> List a
toList = NonEmpty.toList . toNonEmpty

gather :: Set3D a space -> NonEmpty a -> NonEmpty a
gather set accumulated = case set of
  Node _ leftChild rightChild -> gather leftChild (gather rightChild accumulated)
  Leaf _ item -> NonEmpty.push item accumulated

union :: Set3D a space -> Set3D a space -> Set3D a space
union left right = do
  let aggregateBounds = Bounds3D.aggregate2 (bounds left) (bounds right)
  Node aggregateBounds left right

find :: Tolerance Meters => Bounds3D space -> Set3D a space -> Fuzzy (Maybe a)
find searchBounds set = case set of
  Node nodeBounds leftChild rightChild
    | not (nodeBounds `intersects` searchBounds) -> Resolved Nothing -- No overlapping items
    | Bounds3D.contains nodeBounds searchBounds -> Unresolved -- More than one overlapping item
    | otherwise -> case (find searchBounds leftChild, find searchBounds rightChild) of
        (Unresolved, _) -> Unresolved -- More than one item found just in the left
        (_, Unresolved) -> Unresolved -- More than one item found just in the right
        (Resolved Nothing, rightResult) -> rightResult -- If nothing found in the left, use the right result
        (leftResult, Resolved Nothing) -> leftResult -- If nothing found in the right, use the left result
        (Resolved (Just _), Resolved (Just _)) -> Unresolved -- Found exactly one item in each side
  Leaf itemBounds item ->
    Resolved (if searchBounds `intersects` itemBounds then Just item else Nothing)

filter :: Tolerance Meters => Bounds3D space -> Set3D a space -> List a
filter searchBounds set = search searchBounds set []

search :: Tolerance Meters => Bounds3D space -> Set3D a space -> List a -> List a
search searchBounds set accumulated = case set of
  Node nodeBounds leftChild rightChild
    | not (nodeBounds `intersects` searchBounds) -> accumulated
    | Bounds3D.contains nodeBounds searchBounds -> returnAll set accumulated
    | otherwise -> search searchBounds leftChild (search searchBounds rightChild accumulated)
  Leaf itemBounds item ->
    if searchBounds `intersects` itemBounds then item : accumulated else accumulated

returnAll :: Set3D a space -> List a -> List a
returnAll set accumulated = case set of
  Node _ leftChild rightChild -> returnAll leftChild (returnAll rightChild accumulated)
  Leaf _ item -> item : accumulated
