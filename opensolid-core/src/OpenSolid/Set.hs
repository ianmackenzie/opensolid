module OpenSolid.Set
  ( Set (Node, Leaf)
  , size
  , bounds
  , singleton
  , build
  , linear
  , aggregate
  , flatten
  , map
  , reverseMap
  , toNonEmpty
  , toNonEmptyOf
  , toList
  , toListOf
  , union
  , cull
  , cullIndexed
  , foldrMap
  , foldrMapWithIndex
  , foldlMap
  , foldlMapWithIndex
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.IndexOutOfBounds (IndexOutOfBounds (..))
import OpenSolid.InternalError qualified as InternalError
import OpenSolid.Interval qualified as Interval
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

bounds :: Set dimension units space item -> Bounds dimension units space
bounds (SizedNode nodeBounds _ _ _ _) = nodeBounds
bounds (Leaf leafBounds _) = leafBounds

singleton ::
  Bounds.Exists dimension units space =>
  Bounds dimension units space ->
  item ->
  Set dimension units space item
singleton = Leaf

build ::
  Bounds.Exists dimension units space =>
  (item -> Bounds dimension units space) ->
  NonEmpty item ->
  Set dimension units space item
build boundsFunction items = do
  let toLeaf item = Leaf (boundsFunction item) item
  aggregate (NonEmpty.map toLeaf items)

linear ::
  Bounds.Exists dimension units space =>
  (item -> Bounds dimension units space) ->
  NonEmpty item ->
  Set dimension units space item
linear boundsFunction items = do
  let toLeaf item = Leaf (boundsFunction item) item
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

aggregate ::
  Bounds.Exists dimension units space =>
  NonEmpty (Set dimension units space item) ->
  Set dimension units space item
aggregate subsets = aggregateImpl (NonEmpty.length subsets) subsets 0

aggregateImpl ::
  Bounds.Exists dimension units space =>
  Int ->
  NonEmpty (Set dimension units space item) ->
  Int ->
  Set dimension units space item
aggregateImpl count subsets index
  | count == 1 = assert (NonEmpty.length subsets == 1) (NonEmpty.first subsets)
  | otherwise = assert (count >= 2 && NonEmpty.length subsets == count) do
      let indexedCoordinateMidpoint = Interval.midpoint . Bounds.cyclicCoordinate index . bounds
      let sorted = NonEmpty.sortBy indexedCoordinateMidpoint subsets
      let leftCount = count // 2
      let rightCount = count - leftCount
      let (leftSubsets, rightSubsets) = splitAtIndex leftCount sorted
      let leftChild = aggregateImpl leftCount leftSubsets (index + 1)
      let rightChild = aggregateImpl rightCount rightSubsets (index + 1)
      let nodeBounds = Bounds.aggregate2 (bounds leftChild) (bounds rightChild)
      SizedNode nodeBounds leftCount rightCount leftChild rightChild

splitAtIndex :: Int -> NonEmpty a -> (NonEmpty a, NonEmpty a)
splitAtIndex 0 _ = InternalError.throw "Bad split index in Set.aggregateImpl"
splitAtIndex _ (_ :| []) = InternalError.throw "Bad split index in Set.aggregateImpl"
splitAtIndex 1 (first :| NonEmpty rest) = (NonEmpty.one first, rest)
splitAtIndex n (first :| NonEmpty rest) =
  Pair.mapFirst (NonEmpty.push first) (splitAtIndex (n - 1) rest)

flatten :: Set dimension units space (Set dimension units space item) -> Set dimension units space item
flatten (Leaf _ set) = set
flatten (SizedNode nodeBounds _ _ left right) = do
  let flattenedLeft = flatten left
  let flattenedRight = flatten right
  SizedNode nodeBounds (size flattenedLeft) (size flattenedRight) flattenedLeft flattenedRight

map ::
  Bounds.Exists dimension2 units2 space2 =>
  (item1 -> item2) ->
  (item2 -> Bounds dimension2 units2 space2) ->
  Set dimension1 units1 space1 item1 ->
  Set dimension2 units2 space2 item2
map function boundsFunction set = case set of
  Leaf _ item1 -> do
    let item2 = function item1
    Leaf (boundsFunction item2) item2
  SizedNode _ leftSize rightSize leftChild1 rightChild1 -> do
    let leftChild2 = map function boundsFunction leftChild1
    let rightChild2 = map function boundsFunction rightChild1
    let nodeBounds2 = Bounds.aggregate2 (bounds leftChild2) (bounds rightChild2)
    SizedNode nodeBounds2 leftSize rightSize leftChild2 rightChild2

reverseMap ::
  Bounds.Exists dimension2 units2 space2 =>
  (item1 -> item2) ->
  (item2 -> Bounds dimension2 units2 space2) ->
  Set dimension1 units1 space1 item1 ->
  Set dimension2 units2 space2 item2
reverseMap function boundsFunction set = case set of
  Leaf _ item1 -> do
    let item2 = function item1
    Leaf (boundsFunction item2) item2
  SizedNode _ leftSize1 rightSize1 leftChild1 rightChild1 -> do
    let leftSize2 = rightSize1
    let rightSize2 = leftSize1
    let leftChild2 = reverseMap function boundsFunction rightChild1
    let rightChild2 = reverseMap function boundsFunction leftChild1
    let nodeBounds2 = Bounds.aggregate2 (bounds leftChild2) (bounds rightChild2)
    SizedNode nodeBounds2 leftSize2 rightSize2 leftChild2 rightChild2

foldrMap ::
  (item -> accumulated) ->
  (item -> accumulated -> accumulated) ->
  Set dimension units space item ->
  accumulated
foldrMap init function set = foldrMapWithIndex (const init) (const function) set

foldrMapWithIndex ::
  (Int -> item -> accumulated) ->
  (Int -> item -> accumulated -> accumulated) ->
  Set dimension units space item ->
  accumulated
foldrMapWithIndex = foldrMapWithIndexStart 0

foldrMapWithIndexStart ::
  Int ->
  (Int -> item -> accumulated) ->
  (Int -> item -> accumulated -> accumulated) ->
  Set dimension units space item ->
  accumulated
foldrMapWithIndexStart startIndex init function set = case set of
  Leaf _ item -> init startIndex item
  SizedNode _ leftSize _ leftChild rightChild ->
    foldrMapWithIndexStart (startIndex + leftSize) init function rightChild
      & foldrMapWithIndexContinue startIndex function leftChild

foldrMapWithIndexContinue ::
  Int ->
  (Int -> item -> accumulated -> accumulated) ->
  Set dimension units space item ->
  accumulated ->
  accumulated
foldrMapWithIndexContinue startIndex function set accumulated = case set of
  Leaf _ item -> function startIndex item accumulated
  SizedNode _ leftSize _ leftChild rightChild ->
    accumulated
      & foldrMapWithIndexContinue (startIndex + leftSize) function rightChild
      & foldrMapWithIndexContinue startIndex function leftChild

foldlMap ::
  (item -> accumulated) ->
  (accumulated -> item -> accumulated) ->
  Set dimension units space item ->
  accumulated
foldlMap init function set = foldlMapWithIndex (const init) (const function) set

foldlMapWithIndex ::
  (Int -> item -> accumulated) ->
  (Int -> accumulated -> item -> accumulated) ->
  Set dimension units space item ->
  accumulated
foldlMapWithIndex init function set = foldlMapWithIndexStart 0 init function set

foldlMapWithIndexStart ::
  Int ->
  (Int -> item -> accumulated) ->
  (Int -> accumulated -> item -> accumulated) ->
  Set dimension units space item ->
  accumulated
foldlMapWithIndexStart startIndex init function set = case set of
  Leaf _ item -> init startIndex item
  SizedNode _ leftSize _ leftChild rightChild ->
    foldlMapWithIndexStart startIndex init function leftChild
      & foldlMapWithIndexContinue (startIndex + leftSize) function rightChild

foldlMapWithIndexContinue ::
  Int ->
  (Int -> accumulated -> item -> accumulated) ->
  Set dimension units space item ->
  accumulated ->
  accumulated
foldlMapWithIndexContinue startIndex function set accumulated = case set of
  Leaf _ item -> function startIndex accumulated item
  SizedNode _ leftSize _ leftChild rightChild ->
    accumulated
      & foldlMapWithIndexContinue startIndex function leftChild
      & foldlMapWithIndexContinue (startIndex + leftSize) function rightChild

toNonEmpty :: Set dimension units space item -> NonEmpty item
toNonEmpty = foldrMap NonEmpty.one NonEmpty.push

toNonEmptyOf :: (item -> a) -> Set dimension units space item -> NonEmpty a
toNonEmptyOf function = foldrMap (NonEmpty.one . function) (NonEmpty.push . function)

toList :: Set dimension units space item -> List item
toList = NonEmpty.toList . toNonEmpty

toListOf :: (item -> a) -> Set dimension units space item -> List a
toListOf function = NonEmpty.toList . toNonEmptyOf function

union ::
  Bounds.Exists dimension units space =>
  Set dimension units space item ->
  Set dimension units space item ->
  Set dimension units space item
union left right = do
  let aggregateBounds = Bounds.aggregate2 (bounds left) (bounds right)
  SizedNode aggregateBounds (size left) (size right) left right

cull :: (Bounds dimension units space -> Bool) -> Set dimension units space item -> List item
cull predicate set = cullImpl 0 predicate (const id) set []

cullIndexed ::
  (Bounds dimension units space -> Bool) ->
  Set dimension units space item ->
  List (Int, item)
cullIndexed predicate set = cullImpl 0 predicate (,) set []

cullImpl ::
  Int ->
  (Bounds dimension units space -> Bool) ->
  (Int -> item -> a) ->
  Set dimension units space item ->
  List a ->
  List a
cullImpl startIndex predicate function set accumulated =
  case predicate (bounds set) of
    False -> accumulated
    True -> case set of
      Leaf _ item -> function startIndex item : accumulated
      SizedNode _ leftSize _ leftChild rightChild ->
        accumulated
          & cullImpl (startIndex + leftSize) predicate function rightChild
          & cullImpl startIndex predicate function leftChild
