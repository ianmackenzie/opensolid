module OpenSolid.Set2D
  ( Set2D (Node, Leaf)
  , one
  , two
  , fromNonEmpty
  , toNonEmpty
  , toList
  , bounds
  , union
  , find
  , filter
  )
where

import OpenSolid.Bounded2D (Bounded2D)
import OpenSolid.Bounded2D qualified as Bounded2D
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.InternalError (InternalError (InternalError))
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude

data Set2D a units space where
  Node ::
    Bounds2D units space ->
    Set2D a units space ->
    Set2D a units space ->
    Set2D a units space
  Leaf ::
    Bounds2D units space ->
    a ->
    Set2D a units space

instance Bounded2D (Set2D a units space) units space where
  bounds = bounds

bounds :: Set2D a units space -> Bounds2D units space
bounds (Node nodeBounds _ _) = nodeBounds
bounds (Leaf leafBounds _) = leafBounds

one :: Bounded2D a units space => a -> Set2D a units space
one item = Leaf (Bounded2D.bounds item) item

two :: Bounded2D a units space => a -> a -> Set2D a units space
two firstItem secondItem = do
  let firstBounds = Bounded2D.bounds firstItem
  let secondBounds = Bounded2D.bounds secondItem
  let nodeBounds = Bounds2D.aggregate2 firstBounds secondBounds
  Node nodeBounds (Leaf firstBounds firstItem) (Leaf secondBounds secondItem)

fromNonEmpty :: Bounded2D a units space => NonEmpty a -> Set2D a units space
fromNonEmpty givenItems = do
  let boundedItem item = (Bounded2D.bounds item, item)
  let boundedItems = NonEmpty.map boundedItem givenItems
  buildX (NonEmpty.length boundedItems) boundedItems

buildX :: Int -> NonEmpty (Bounds2D units space, a) -> Set2D a units space
buildX = build Bounds2D.xCoordinate buildY

buildY :: Int -> NonEmpty (Bounds2D units space, a) -> Set2D a units space
buildY = build Bounds2D.yCoordinate buildX

build ::
  (Bounds2D units space -> Interval units) ->
  (Int -> NonEmpty (Bounds2D units space, a) -> Set2D a units space) ->
  Int ->
  NonEmpty (Bounds2D units space, a) ->
  Set2D a units space
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
      let nodeBounds = Bounds2D.aggregate2 (bounds leftChild) (bounds rightChild)
      Node nodeBounds leftChild rightChild

splitAtIndex :: Int -> NonEmpty a -> (NonEmpty a, NonEmpty a)
splitAtIndex 0 _ = throw (InternalError "Bad split index in Set2D.new")
splitAtIndex _ (_ :| []) = throw (InternalError "Bad split index in Set2D.new")
splitAtIndex 1 (first :| NonEmpty rest) = (NonEmpty.one first, rest)
splitAtIndex n (first :| NonEmpty rest) =
  Pair.mapFirst (NonEmpty.push first) (splitAtIndex (n - 1) rest)

toNonEmpty :: Set2D a units space -> NonEmpty a
toNonEmpty (Node _ leftChild rightChild) = gather leftChild (toNonEmpty rightChild)
toNonEmpty (Leaf _ item) = NonEmpty.one item

toList :: Set2D a units space -> List a
toList = NonEmpty.toList . toNonEmpty

gather :: Set2D a units space -> NonEmpty a -> NonEmpty a
gather set accumulated = case set of
  Node _ leftChild rightChild -> gather leftChild (gather rightChild accumulated)
  Leaf _ item -> NonEmpty.push item accumulated

union :: Set2D a units space -> Set2D a units space -> Set2D a units space
union left right = do
  let aggregateBounds = Bounds2D.aggregate2 (bounds left) (bounds right)
  Node aggregateBounds left right

find :: Tolerance units => Bounds2D units space -> Set2D a units space -> Fuzzy (Maybe a)
find searchBounds set = case set of
  Node nodeBounds leftChild rightChild
    | not (nodeBounds `intersects` searchBounds) -> Resolved Nothing -- No overlapping items
    | Bounds2D.contains nodeBounds searchBounds -> Unresolved -- More than one overlapping item
    | otherwise -> case (find searchBounds leftChild, find searchBounds rightChild) of
        (Unresolved, _) -> Unresolved -- More than one item found just in the left
        (_, Unresolved) -> Unresolved -- More than one item found just in the right
        (Resolved Nothing, rightResult) -> rightResult -- If nothing found in the left, use the right result
        (leftResult, Resolved Nothing) -> leftResult -- If nothing found in the right, use the left result
        (Resolved (Just _), Resolved (Just _)) -> Unresolved -- Found exactly one item in each side
  Leaf itemBounds item ->
    Resolved (if searchBounds `intersects` itemBounds then Just item else Nothing)

filter :: Tolerance units => Bounds2D units space -> Set2D a units space -> List a
filter searchBounds set = search searchBounds set []

search :: Tolerance units => Bounds2D units space -> Set2D a units space -> List a -> List a
search searchBounds set accumulated = case set of
  Node nodeBounds leftChild rightChild
    | not (nodeBounds `intersects` searchBounds) -> accumulated
    | Bounds2D.contains nodeBounds searchBounds -> returnAll set accumulated
    | otherwise -> search searchBounds leftChild (search searchBounds rightChild accumulated)
  Leaf itemBounds item ->
    if searchBounds `intersects` itemBounds then item : accumulated else accumulated

returnAll :: Set2D a units space -> List a -> List a
returnAll set accumulated = case set of
  Node _ leftChild rightChild -> returnAll leftChild (returnAll rightChild accumulated)
  Leaf _ item -> item : accumulated
