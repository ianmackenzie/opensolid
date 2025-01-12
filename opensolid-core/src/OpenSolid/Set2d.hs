module OpenSolid.Set2d
  ( Set2d
  , pattern One
  , pattern Two
  , one
  , two
  , fromNonEmpty
  , size
  , toNonEmpty
  , toList
  , union
  , find
  , filter
  )
where

import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Debug qualified as Debug
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range

data Set2d a (coordinateSystem :: CoordinateSystem) where
  Node ::
    Int ->
    (Bounds2d (space @ units)) ->
    Set2d a (space @ units) ->
    Set2d a (space @ units) ->
    Set2d a (space @ units)
  Leaf ::
    (Bounds2d (space @ units)) ->
    a ->
    Set2d a (space @ units)

instance Bounded (Set2d a (space @ units)) (Bounds2d (space @ units)) where
  bounds = bounds

pattern One :: Bounded a (Bounds2d (space @ units)) => a -> Set2d a (space @ units)
pattern One item <- Leaf _ item

pattern Two :: Bounded a (Bounds2d (space @ units)) => a -> a -> Set2d a (space @ units)
pattern Two firstItem secondItem <- Node _ _ (Leaf _ firstItem) (Leaf _ secondItem)

bounds :: Set2d a (space @ units) -> Bounds2d (space @ units)
bounds (Node _ nodeBounds _ _) = nodeBounds
bounds (Leaf leafBounds _) = leafBounds

one :: Bounded a (Bounds2d (space @ units)) => a -> Set2d a (space @ units)
one item = Leaf (Bounded.bounds item) item

two :: Bounded a (Bounds2d (space @ units)) => a -> a -> Set2d a (space @ units)
two firstItem secondItem = do
  let firstBounds = Bounded.bounds firstItem
  let secondBounds = Bounded.bounds secondItem
  let nodeBounds = Bounds2d.aggregate2 firstBounds secondBounds
  Node 2 nodeBounds (Leaf firstBounds firstItem) (Leaf secondBounds secondItem)

fromNonEmpty :: Bounded a (Bounds2d (space @ units)) => NonEmpty a -> Set2d a (space @ units)
fromNonEmpty givenItems = do
  let boundedItem item = (Bounded.bounds item, item)
  let boundedItems = NonEmpty.map boundedItem givenItems
  buildX (NonEmpty.length boundedItems) boundedItems

buildX :: Int -> NonEmpty (Bounds2d (space @ units), a) -> Set2d a (space @ units)
buildX = build Bounds2d.xCoordinate buildY

buildY :: Int -> NonEmpty (Bounds2d (space @ units), a) -> Set2d a (space @ units)
buildY = build Bounds2d.yCoordinate buildX

build ::
  (Bounds2d (space @ units) -> Range units) ->
  (Int -> NonEmpty (Bounds2d (space @ units), a) -> Set2d a (space @ units)) ->
  Int ->
  NonEmpty (Bounds2d (space @ units), a) ->
  Set2d a (space @ units)
build boundsCoordinate buildSubset n boundedItems
  | n == 1 = do
      Debug.assert (NonEmpty.length boundedItems == 1)
      let (itemBounds, item) = NonEmpty.first boundedItems
      Leaf itemBounds item
  | otherwise = do
      Debug.assert (n >= 2)
      Debug.assert (NonEmpty.length boundedItems == n)
      let sorted = NonEmpty.sortBy (Range.midpoint . boundsCoordinate . Pair.first) boundedItems
      let leftN = n // 2
      let rightN = n - leftN
      let (leftBoundedItems, rightBoundedItems) = splitAt leftN sorted
      let leftChild = buildSubset leftN leftBoundedItems
      let rightChild = buildSubset rightN rightBoundedItems
      let nodeBounds = Bounds2d.aggregate2 (bounds leftChild) (bounds rightChild)
      Node n nodeBounds leftChild rightChild

splitAt :: Int -> NonEmpty a -> (NonEmpty a, NonEmpty a)
splitAt 0 _ = internalError "Bad split index in Set2d.new"
splitAt _ (_ :| []) = internalError "Bad split index in Set2d.new"
splitAt 1 (first :| NonEmpty rest) = (NonEmpty.one first, rest)
splitAt n (first :| NonEmpty rest) = Pair.mapFirst (NonEmpty.prepend first) (splitAt (n - 1) rest)

size :: Set2d a (space @ units) -> Int
size (Node n _ _ _) = n
size (Leaf _ _) = 1

toNonEmpty :: Set2d a (space @ units) -> NonEmpty a
toNonEmpty (Node _ _ leftChild rightChild) = gather leftChild (toNonEmpty rightChild)
toNonEmpty (Leaf _ item) = NonEmpty.one item

toList :: Set2d a (space @ units) -> List a
toList = toNonEmpty >> NonEmpty.toList

gather :: Set2d a (space @ units) -> NonEmpty a -> NonEmpty a
gather set accumulated = case set of
  Node _ _ leftChild rightChild -> gather leftChild (gather rightChild accumulated)
  Leaf _ item -> NonEmpty.prepend item accumulated

union :: Set2d a (space @ units) -> Set2d a (space @ units) -> Set2d a (space @ units)
union left right = do
  let aggregateSize = size left + size right
  let aggregateBounds = Bounds2d.aggregate2 (bounds left) (bounds right)
  Node aggregateSize aggregateBounds left right

find :: Tolerance units => Bounds2d (space @ units) -> Set2d a (space @ units) -> Fuzzy (Maybe a)
find searchBounds set = case set of
  Node _ nodeBounds leftChild rightChild
    | not (nodeBounds ^ searchBounds) -> Resolved Nothing -- No overlapping items
    | Bounds2d.contains nodeBounds searchBounds -> Unresolved -- More than one overlapping item
    | otherwise -> case (find searchBounds leftChild, find searchBounds rightChild) of
        (Unresolved, _) -> Unresolved -- More than one item found just in the left
        (_, Unresolved) -> Unresolved -- More than one item found just in the right
        (Resolved Nothing, rightResult) -> rightResult -- If nothing found in the left, use the right result
        (leftResult, Resolved Nothing) -> leftResult -- If nothing found in the right, use the left result
        (Resolved (Just _), Resolved (Just _)) -> Unresolved -- Found exactly one item in each side
  Leaf itemBounds item -> Resolved (if searchBounds ^ itemBounds then Just item else Nothing)

filter :: Tolerance units => Bounds2d (space @ units) -> Set2d a (space @ units) -> List a
filter searchBounds set = search searchBounds set []

search :: Tolerance units => Bounds2d (space @ units) -> Set2d a (space @ units) -> List a -> List a
search searchBounds set accumulated = case set of
  Node _ nodeBounds leftChild rightChild
    | not (nodeBounds ^ searchBounds) -> accumulated
    | Bounds2d.contains nodeBounds searchBounds -> NonEmpty.toList (toNonEmpty set)
    | otherwise -> search searchBounds leftChild (search searchBounds rightChild accumulated)
  Leaf itemBounds item -> if searchBounds ^ itemBounds then item : accumulated else accumulated