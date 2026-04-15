module OpenSolid.Set
  ( Set (Node, Leaf)
  , size
  , bounds
  , one
  , two
  , partition
  , partitionBy
  , linear
  , linearBy
  , toNonEmpty
  , toList
  , union
  , find
  , findWithIndex
  , findAll
  , findAllWithIndices
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Fuzzy qualified as Fuzzy
import OpenSolid.IndexOutOfBounds (IndexOutOfBounds (..))
import OpenSolid.InternalError qualified as InternalError
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units

data Set dimension units space item where
  Leaf ::
    Bounds dimension units space ->
    item ->
    Set dimension units space item
  SizedNode ::
    Bounds dimension units space ->
    Int ->
    Int ->
    Set dimension units space item ->
    Set dimension units space item ->
    Set dimension units space item

deriving instance
  (Bounds.Exists dimension units space, Show item) =>
  Show (Set dimension units space item)

instance
  ( dimension1 ~ dimension2
  , space1 ~ space2
  , Units.Coercion (Bounds dimension1 units1 space1) (Bounds dimension2 units2 space2)
  , Units.Coercion item1 item2
  , HasUnits item1 units1
  , HasUnits item2 units2
  ) =>
  Units.Coercion (Set dimension1 units1 space1 item1) (Set dimension2 units2 space2 item2)
  where
  coerce (Leaf itemBounds item) = Leaf (Units.coerce itemBounds) (Units.coerce item)
  coerce (SizedNode nodeBounds leftSize rightSize leftChild rightChild) =
    SizedNode
      (Units.coerce nodeBounds)
      leftSize
      rightSize
      (Units.coerce leftChild)
      (Units.coerce rightChild)

instance Indexed (Set dimension units space item) Int item where
  set !! index
    | index >= 0 && index <= size set = get index set
    | otherwise = throw IndexOutOfBounds{index = index, size = size set}

get :: Int -> Set dimension units space item -> item
get index set = case set of
  SizedNode _ leftSize _ leftChild rightChild
    | index < leftSize -> get index leftChild
    | otherwise -> get (index - leftSize) rightChild
  Leaf _ item -> assert (index == 0) item

pattern Node ::
  Bounds dimension units space ->
  Set dimension units space item ->
  Set dimension units space item ->
  Set dimension units space item
pattern Node nodeBounds leftChild rightChild <- SizedNode nodeBounds _ _ leftChild rightChild
  where
    Node nodeBounds leftChild rightChild =
      SizedNode nodeBounds (size leftChild) (size rightChild) leftChild rightChild

size :: Set dimension units space item -> Int
size Leaf{} = 1
size (SizedNode _ leftSize rightSize _ _) = leftSize + rightSize

bounds ::
  Bounds.Exists dimension units space =>
  Set dimension units space item ->
  Bounds dimension units space
bounds (SizedNode nodeBounds _ _ _ _) = nodeBounds
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
  SizedNode nodeBounds 1 1 (Leaf firstBounds firstItem) (Leaf secondBounds secondItem)

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
      SizedNode nodeBounds leftCount rightCount leftChild rightChild

linear ::
  Bounds.Exists dimension units space =>
  NonEmpty (item, Bounds dimension units space) ->
  Set dimension units space item
linear boundedItems = do
  let toLeaf (item, itemBounds) = Leaf itemBounds item
  buildLinear (NonEmpty.map toLeaf boundedItems)

linearBy ::
  Bounds.Exists dimension units space =>
  (item -> Bounds dimension units space) ->
  NonEmpty item ->
  Set dimension units space item
linearBy function items = do
  let toLeaf item = Leaf (function item) item
  buildLinear (NonEmpty.map toLeaf items)

buildLinear ::
  Bounds.Exists dimension units space =>
  NonEmpty (Set dimension units space item) ->
  Set dimension units space item
buildLinear sets =
  case reduceLinear sets of
    NonEmpty.One set -> set
    reduced -> buildLinear reduced

reduceLinear ::
  Bounds.Exists dimension units space =>
  NonEmpty (Set dimension units space item) ->
  NonEmpty (Set dimension units space item)
reduceLinear (first :| []) = NonEmpty.one first
reduceLinear (first :| second : []) = NonEmpty.one (union first second)
reduceLinear (first :| second : NonEmpty rest) =
  NonEmpty.push (union first second) (reduceLinear rest)

splitAtIndex :: Int -> NonEmpty a -> (NonEmpty a, NonEmpty a)
splitAtIndex 0 _ = InternalError.throw "Bad split index in Set.build"
splitAtIndex _ (_ :| []) = InternalError.throw "Bad split index in Set.build"
splitAtIndex 1 (first :| NonEmpty rest) = (NonEmpty.one first, rest)
splitAtIndex n (first :| NonEmpty rest) =
  Pair.mapFirst (NonEmpty.push first) (splitAtIndex (n - 1) rest)

toNonEmpty :: Set dimension units space item -> NonEmpty item
toNonEmpty (SizedNode _ _ _ leftChild rightChild) = gather leftChild (toNonEmpty rightChild)
toNonEmpty (Leaf _ item) = NonEmpty.one item

toList :: Set dimension units space item -> List item
toList = NonEmpty.toList . toNonEmpty

gather :: Set dimension units space item -> NonEmpty item -> NonEmpty item
gather set accumulated = case set of
  SizedNode _ _ _ leftChild rightChild -> gather leftChild (gather rightChild accumulated)
  Leaf _ item -> NonEmpty.push item accumulated

union ::
  Bounds.Exists dimension units space =>
  Set dimension units space item ->
  Set dimension units space item ->
  Set dimension units space item
union left right = do
  let aggregateBounds = Bounds.aggregate2 (bounds left) (bounds right)
  SizedNode aggregateBounds (size left) (size right) left right

find ::
  (Bounds.Exists dimension units space, Tolerance units) =>
  Bounds dimension units space ->
  Set dimension units space item ->
  Fuzzy (Maybe item)
find searchBounds set = Fuzzy.map (Maybe.map Pair.second) (findWithIndex searchBounds set)

findWithIndex ::
  (Bounds.Exists dimension units space, Tolerance units) =>
  Bounds dimension units space ->
  Set dimension units space item ->
  Fuzzy (Maybe (Int, item))
findWithIndex searchBounds set = findWithIndexImpl 0 searchBounds set

findWithIndexImpl ::
  (Bounds.Exists dimension units space, Tolerance units) =>
  Int ->
  Bounds dimension units space ->
  Set dimension units space item ->
  Fuzzy (Maybe (Int, item))
findWithIndexImpl startIndex searchBounds set = case set of
  SizedNode nodeBounds leftSize _ leftChild rightChild
    | not (nodeBounds `intersects` searchBounds) -> Resolved Nothing -- No overlapping items
    | Bounds.contains nodeBounds searchBounds -> Unresolved -- More than one overlapping item
    | otherwise -> do
        let leftResult = findWithIndexImpl startIndex searchBounds leftChild
        let rightResult = findWithIndexImpl (startIndex + leftSize) searchBounds rightChild
        case (leftResult, rightResult) of
          (Unresolved, _) -> Unresolved -- More than one item found just in the left
          (_, Unresolved) -> Unresolved -- More than one item found just in the right
          (Resolved Nothing, _) -> rightResult -- If nothing found in the left, use the right result
          (_, Resolved Nothing) -> leftResult -- If nothing found in the right, use the left result
          (Resolved (Just _), Resolved (Just _)) -> Unresolved -- Found exactly one item in each side
  Leaf itemBounds item ->
    Resolved (if searchBounds `intersects` itemBounds then Just (startIndex, item) else Nothing)

findAll ::
  (Bounds.Exists dimension units space, Tolerance units) =>
  Bounds dimension units space ->
  Set dimension units space item ->
  List item
findAll searchBounds set = List.map Pair.second (findAllWithIndices searchBounds set)

findAllWithIndices ::
  (Bounds.Exists dimension units space, Tolerance units) =>
  Bounds dimension units space ->
  Set dimension units space item ->
  List (Int, item)
findAllWithIndices searchBounds set = findAllWithIndicesImpl 0 searchBounds set []

findAllWithIndicesImpl ::
  (Bounds.Exists dimension units space, Tolerance units) =>
  Int ->
  Bounds dimension units space ->
  Set dimension units space item ->
  List (Int, item) ->
  List (Int, item)
findAllWithIndicesImpl startIndex searchBounds set accumulated = case set of
  SizedNode nodeBounds leftSize _ leftChild rightChild
    | not (nodeBounds `intersects` searchBounds) -> accumulated
    | Bounds.contains nodeBounds searchBounds -> returnAll startIndex set accumulated
    | otherwise ->
        accumulated
          & findAllWithIndicesImpl (startIndex + leftSize) searchBounds rightChild
          & findAllWithIndicesImpl startIndex searchBounds leftChild
  Leaf itemBounds item ->
    if searchBounds `intersects` itemBounds then (startIndex, item) : accumulated else accumulated

returnAll :: Int -> Set dimension units space item -> List (Int, item) -> List (Int, item)
returnAll startIndex set accumulated = case set of
  SizedNode _ leftSize _ leftChild rightChild ->
    accumulated
      & returnAll (startIndex + leftSize) rightChild
      & returnAll startIndex leftChild
  Leaf _ item -> (startIndex, item) : accumulated
