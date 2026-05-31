module OpenSolid.Set
  ( Set (..)
  , size
  , bounds
  , singleton
  , build
  , linear
  , aggregate
  , flatten
  , map
  , mapWithIndex
  , reverseMap
  , combine
  , combineWithIndex
  , toNonEmpty
  , toNonEmptyOf
  , toNonEmptyWithIndex
  , toList
  , toListOf
  , toListWithIndex
  , union
  , cull
  , filter
  , filterMap
  , filterWithIndex
  , filterMapWithIndex
  , subset
  , any
  , all
  , forEach
  , forEachWithIndex
  , reverseForEach
  , reverseForEachWithIndex
  , pairwiseFilter
  , pairwiseFilterMap
  , pairwiseFilterWithIndices
  , pairwiseFilterMapWithIndices
  , clusters
  , foldr
  , foldrWithIndex
  , foldl
  , foldlWithIndex
  , foldrMap
  , foldrMapWithIndex
  , foldlMap
  , foldlMapWithIndex
  )
where

import Data.Graph qualified as Graph
import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Chainable (Chainable)
import OpenSolid.Chainable qualified as Chainable
import OpenSolid.IndexOutOfBounds (IndexOutOfBounds (..))
import OpenSolid.InternalError qualified as InternalError
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import Prelude qualified

data Set dimension units space item where
  Leaf ::
    { leafBounds :: Bounds dimension units space
    , leafItem :: item
    } ->
    Set dimension units space item
  Node ::
    { nodeBounds :: Bounds dimension units space
    , leftSize :: Int
    , rightSize :: Int
    , leftChild :: Set dimension units space item
    , rightChild :: Set dimension units space item
    } ->
    Set dimension units space item

deriving instance
  (Bounds.Exists dimension units space, Show item) =>
  Show (Set dimension units space item)

instance Foldable (Set dimension units space) where
  foldl = foldl
  foldr = foldr

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
  coerce Leaf{leafBounds, leafItem} =
    Leaf
      { leafBounds = Units.coerce leafBounds
      , leafItem = Units.coerce leafItem
      }
  coerce Node{nodeBounds, leftSize, rightSize, leftChild, rightChild} =
    Node
      { nodeBounds = Units.coerce nodeBounds
      , leftSize
      , rightSize
      , leftChild = Units.coerce leftChild
      , rightChild = Units.coerce rightChild
      }

instance Indexed (Set dimension units space item) Int item where
  set !! index
    | index >= 0 && index <= size set = get index set
    | otherwise = throw IndexOutOfBounds{index = index, size = size set}

get :: Int -> Set dimension units space item -> item
get index set = case set of
  Node{leftSize, leftChild, rightChild}
    | index < leftSize -> get index leftChild
    | otherwise -> get (index - leftSize) rightChild
  Leaf{leafItem} -> assert (index == 0) leafItem

getLeaf :: Int -> Set dimension units space item -> Set dimension units space item
getLeaf index set = case set of
  Node{leftSize, leftChild, rightChild}
    | index < leftSize -> getLeaf index leftChild
    | otherwise -> getLeaf (index - leftSize) rightChild
  Leaf{} -> assert (index == 0) set

size :: Set dimension units space item -> Int
size Leaf{} = 1
size Node{leftSize, rightSize} = leftSize + rightSize

bounds :: Set dimension units space item -> Bounds dimension units space
bounds Node{nodeBounds} = nodeBounds
bounds Leaf{leafBounds} = leafBounds

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
      let leftSize = count // 2
      let rightSize = count - leftSize
      let (leftSubsets, rightSubsets) = splitAtIndex leftSize sorted
      let leftChild = aggregateImpl leftSize leftSubsets (index + 1)
      let rightChild = aggregateImpl rightSize rightSubsets (index + 1)
      let nodeBounds = Bounds.aggregate2 (bounds leftChild) (bounds rightChild)
      Node{nodeBounds, leftSize, rightSize, leftChild, rightChild}

splitAtIndex :: Int -> NonEmpty a -> (NonEmpty a, NonEmpty a)
splitAtIndex 0 _ = InternalError.throw "Bad split index in Set.aggregateImpl"
splitAtIndex _ (_ :| []) = InternalError.throw "Bad split index in Set.aggregateImpl"
splitAtIndex 1 (first :| NonEmpty rest) = (NonEmpty.one first, rest)
splitAtIndex n (first :| NonEmpty rest) =
  Pair.mapFirst (NonEmpty.push first) (splitAtIndex (n - 1) rest)

flatten :: Set dimension units space (Set dimension units space item) -> Set dimension units space item
flatten Leaf{leafItem} = leafItem
flatten Node{nodeBounds, leftChild, rightChild} = do
  let flattenedLeft = flatten leftChild
  let flattenedRight = flatten rightChild
  Node
    { nodeBounds
    , leftSize = size flattenedLeft
    , rightSize = size flattenedRight
    , leftChild = flattenedLeft
    , rightChild = flattenedRight
    }

map ::
  Bounds.Exists dimension2 units2 space2 =>
  (item1 -> item2) ->
  (item2 -> Bounds dimension2 units2 space2) ->
  Set dimension1 units1 space1 item1 ->
  Set dimension2 units2 space2 item2
map function boundsFunction set = mapWithIndex (const function) boundsFunction set

mapWithIndex ::
  Bounds.Exists dimension2 units2 space2 =>
  (Int -> item1 -> item2) ->
  (item2 -> Bounds dimension2 units2 space2) ->
  Set dimension1 units1 space1 item1 ->
  Set dimension2 units2 space2 item2
mapWithIndex function boundsFunction =
  combineWithIndex \index item1 -> do
    let item2 = function index item1
    let bounds2 = boundsFunction item2
    singleton bounds2 item2

reverseMap ::
  Bounds.Exists dimension2 units2 space2 =>
  (item1 -> item2) ->
  (item2 -> Bounds dimension2 units2 space2) ->
  Set dimension1 units1 space1 item1 ->
  Set dimension2 units2 space2 item2
reverseMap function boundsFunction set = case set of
  Leaf{leafItem} -> do
    let mappedItem = function leafItem
    let mappedBounds = boundsFunction mappedItem
    Leaf{leafBounds = mappedBounds, leafItem = mappedItem}
  Node{leftChild, rightChild} -> do
    let reverseMappedLeft = reverseMap function boundsFunction leftChild
    let reverseMappedRight = reverseMap function boundsFunction rightChild
    union reverseMappedRight reverseMappedLeft

combine ::
  Bounds.Exists dimension2 units2 space2 =>
  (item1 -> Set dimension2 units2 space2 item2) ->
  Set dimension1 units1 space1 item1 ->
  Set dimension2 units2 space2 item2
combine function set = combineWithIndex (const function) set

combineWithIndex ::
  Bounds.Exists dimension2 units2 space2 =>
  (Int -> item1 -> Set dimension2 units2 space2 item2) ->
  Set dimension1 units1 space1 item1 ->
  Set dimension2 units2 space2 item2
combineWithIndex function set = combineWithIndexImpl 0 function set

combineWithIndexImpl ::
  Bounds.Exists dimension2 units2 space2 =>
  Int ->
  (Int -> item1 -> Set dimension2 units2 space2 item2) ->
  Set dimension1 units1 space1 item1 ->
  Set dimension2 units2 space2 item2
combineWithIndexImpl startIndex function set = case set of
  Leaf{leafItem} -> function startIndex leafItem
  Node{leftSize, leftChild, rightChild} -> do
    let combinedLeft = combineWithIndexImpl startIndex function leftChild
    let combinedRight = combineWithIndexImpl (startIndex + leftSize) function rightChild
    union combinedLeft combinedRight

forEach ::
  Chainable action =>
  Set dimension units space item ->
  (item -> action) ->
  action
forEach set function = forEachWithIndex set (const function)

forEachWithIndex ::
  Chainable action =>
  Set dimension units space item ->
  (Int -> item -> action) ->
  action
forEachWithIndex = forEachWithIndexImpl 0

forEachWithIndexImpl ::
  Chainable action =>
  Int ->
  Set dimension units space item ->
  (Int -> item -> action) ->
  action
forEachWithIndexImpl startIndex set function = case set of
  Leaf{leafItem} -> function startIndex leafItem
  Node{leftSize, leftChild, rightChild} -> do
    let leftAction = forEachWithIndexImpl startIndex leftChild function
    let rightAction = forEachWithIndexImpl (startIndex + leftSize) rightChild function
    Chainable.chain leftAction rightAction

reverseForEach ::
  Chainable action =>
  Set dimension units space item ->
  (item -> action) ->
  action
reverseForEach set function = reverseForEachWithIndex set (const function)

reverseForEachWithIndex ::
  Chainable action =>
  Set dimension units space item ->
  (Int -> item -> action) ->
  action
reverseForEachWithIndex = reverseForEachWithIndexImpl 0

reverseForEachWithIndexImpl ::
  Chainable action =>
  Int ->
  Set dimension units space item ->
  (Int -> item -> action) ->
  action
reverseForEachWithIndexImpl startIndex set function = case set of
  Leaf{leafItem} -> function startIndex leafItem
  Node{leftSize, leftChild, rightChild} -> do
    let rightAction = reverseForEachWithIndexImpl (startIndex + leftSize) rightChild function
    let leftAction = reverseForEachWithIndexImpl startIndex leftChild function
    Chainable.chain rightAction leftAction

foldr ::
  (item -> accumulated -> accumulated) ->
  accumulated ->
  Set dimension units space item ->
  accumulated
foldr function init set = foldrWithIndex (const function) init set

foldl ::
  (accumulated -> item -> accumulated) ->
  accumulated ->
  Set dimension units space item ->
  accumulated
foldl function init set = foldlWithIndex (const function) init set

foldrWithIndex ::
  (Int -> item -> accumulated -> accumulated) ->
  accumulated ->
  Set dimension units space item ->
  accumulated
foldrWithIndex function init set = reverseForEachWithIndex set function init

foldlWithIndex ::
  (Int -> accumulated -> item -> accumulated) ->
  accumulated ->
  Set dimension units space item ->
  accumulated
foldlWithIndex function init set =
  init & forEachWithIndex set \index item accumulated -> function index accumulated item

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
  Leaf{leafItem} -> init startIndex leafItem
  Node{leftSize, leftChild, rightChild} ->
    foldrMapWithIndexStart (startIndex + leftSize) init function rightChild
      & foldrMapWithIndexContinue startIndex function leftChild

foldrMapWithIndexContinue ::
  Int ->
  (Int -> item -> accumulated -> accumulated) ->
  Set dimension units space item ->
  accumulated ->
  accumulated
foldrMapWithIndexContinue startIndex function set accumulated = case set of
  Leaf{leafItem} -> function startIndex leafItem accumulated
  Node{leftSize, leftChild, rightChild} ->
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
  Leaf{leafItem} -> init startIndex leafItem
  Node{leftSize, leftChild, rightChild} ->
    foldlMapWithIndexStart startIndex init function leftChild
      & foldlMapWithIndexContinue (startIndex + leftSize) function rightChild

foldlMapWithIndexContinue ::
  Int ->
  (Int -> accumulated -> item -> accumulated) ->
  Set dimension units space item ->
  accumulated ->
  accumulated
foldlMapWithIndexContinue startIndex function set accumulated = case set of
  Leaf{leafItem} -> function startIndex accumulated leafItem
  Node{leftSize, leftChild, rightChild} ->
    accumulated
      & foldlMapWithIndexContinue startIndex function leftChild
      & foldlMapWithIndexContinue (startIndex + leftSize) function rightChild

toNonEmpty :: Set dimension units space item -> NonEmpty item
toNonEmpty = foldrMap NonEmpty.one NonEmpty.push

toNonEmptyOf :: (item -> a) -> Set dimension units space item -> NonEmpty a
toNonEmptyOf function = foldrMap (NonEmpty.one . function) (NonEmpty.push . function)

toNonEmptyWithIndex :: (Int -> item -> a) -> Set dimension units space item -> NonEmpty a
toNonEmptyWithIndex function =
  foldrMapWithIndex
    (\index item -> NonEmpty.one (function index item))
    (\index item accumulated -> NonEmpty.push (function index item) accumulated)

toList :: Set dimension units space item -> List item
toList = NonEmpty.toList . toNonEmpty

toListOf :: (item -> a) -> Set dimension units space item -> List a
toListOf function = NonEmpty.toList . toNonEmptyOf function

toListWithIndex :: (Int -> item -> a) -> Set dimension units space item -> List a
toListWithIndex function = NonEmpty.toList . toNonEmptyWithIndex function

union ::
  Bounds.Exists dimension units space =>
  Set dimension units space item ->
  Set dimension units space item ->
  Set dimension units space item
union leftChild rightChild =
  Node
    { nodeBounds = Bounds.aggregate2 (bounds leftChild) (bounds rightChild)
    , leftSize = size leftChild
    , rightSize = size rightChild
    , leftChild
    , rightChild
    }

cull :: (Bounds dimension units space -> Bool) -> Set dimension units space item -> List item
cull boundsPredicate set = filterMap boundsPredicate Just set

filter ::
  (Bounds dimension units space -> Bool) ->
  (item -> Bool) ->
  Set dimension units space item ->
  List item
filter boundsPredicate itemPredicate set =
  filterWithIndex boundsPredicate (const itemPredicate) set

filterMap ::
  (Bounds dimension units space -> Bool) ->
  (item -> Maybe a) ->
  Set dimension units space item ->
  List a
filterMap boundsPredicate callback set =
  filterMapWithIndex boundsPredicate (const callback) set

filterWithIndex ::
  (Bounds dimension units space -> Bool) ->
  (Int -> item -> Bool) ->
  Set dimension units space item ->
  List item
filterWithIndex boundsPredicate itemPredicate set = do
  let callback index item = if itemPredicate index item then Just item else Nothing
  filterMapWithIndex boundsPredicate callback set

filterMapWithIndex ::
  (Bounds dimension units space -> Bool) ->
  (Int -> item -> Maybe a) ->
  Set dimension units space item ->
  List a
filterMapWithIndex boundsPredicate callback set =
  filterMapWithIndexImpl 0 boundsPredicate callback set []

filterMapWithIndexImpl ::
  Int ->
  (Bounds dimension units space -> Bool) ->
  (Int -> item -> Maybe a) ->
  Set dimension units space item ->
  List a ->
  List a
filterMapWithIndexImpl startIndex boundsPredicate callback set accumulated = case set of
  Leaf{leafBounds, leafItem} ->
    if boundsPredicate leafBounds
      then case callback startIndex leafItem of
        Just result -> result : accumulated
        Nothing -> accumulated
      else accumulated
  Node{nodeBounds, leftSize, leftChild, rightChild} ->
    if boundsPredicate nodeBounds
      then
        accumulated
          & filterMapWithIndexImpl (startIndex + leftSize) boundsPredicate callback rightChild
          & filterMapWithIndexImpl startIndex boundsPredicate callback leftChild
      else accumulated

subset ::
  Bounds.Exists dimension units space =>
  (Bounds dimension units space -> Bool) ->
  (item -> Bool) ->
  Set dimension units space item ->
  Maybe (Set dimension units space item)
subset boundsPredicate itemPredicate set = case set of
  Leaf{leafBounds, leafItem} ->
    if boundsPredicate leafBounds && itemPredicate leafItem then Just set else Nothing
  Node{nodeBounds, leftChild, rightChild} ->
    if boundsPredicate nodeBounds
      then do
        let maybeLeft = subset boundsPredicate itemPredicate leftChild
        let maybeRight = subset boundsPredicate itemPredicate rightChild
        joinMaybes maybeLeft maybeRight
      else Nothing

joinMaybes ::
  Bounds.Exists dimension units space =>
  Maybe (Set dimension units space item) ->
  Maybe (Set dimension units space item) ->
  Maybe (Set dimension units space item)
joinMaybes (Just left) (Just right) = Just (union left right)
joinMaybes (Just left) Nothing = Just left
joinMaybes Nothing (Just right) = Just right
joinMaybes Nothing Nothing = Nothing

any ::
  (Bounds dimension units space -> Bool) ->
  (item -> Bool) ->
  Set dimension units space item ->
  Bool
any boundsPredicate itemPredicate set = case set of
  Leaf{leafBounds, leafItem} -> boundsPredicate leafBounds && itemPredicate leafItem
  Node{nodeBounds, leftChild, rightChild} ->
    boundsPredicate nodeBounds && do
      any boundsPredicate itemPredicate leftChild || any boundsPredicate itemPredicate rightChild

all ::
  (Bounds dimension units space -> Bool) ->
  (item -> Bool) ->
  Set dimension units space item ->
  Bool
all boundsPredicate itemPredicate set = case set of
  Leaf{leafBounds, leafItem} -> boundsPredicate leafBounds && itemPredicate leafItem
  Node{nodeBounds, leftChild, rightChild} ->
    boundsPredicate nodeBounds && do
      all boundsPredicate itemPredicate leftChild && all boundsPredicate itemPredicate rightChild

pairwiseFilter ::
  (Bounds dimension1 units1 space1 -> Bounds dimension2 units2 space2 -> Bool) ->
  (item1 -> item2 -> Bool) ->
  Set dimension1 units1 space1 item1 ->
  Set dimension2 units2 space2 item2 ->
  List (item1, item2)
pairwiseFilter boundsPredicate itemPredicate set1 set2 = do
  let callback item1 item2 = if itemPredicate item1 item2 then Just (item1, item2) else Nothing
  pairwiseFilterMap boundsPredicate callback set1 set2

pairwiseFilterMap ::
  (Bounds dimension1 units1 space1 -> Bounds dimension2 units2 space2 -> Bool) ->
  (item1 -> item2 -> Maybe a) ->
  Set dimension1 units1 space1 item1 ->
  Set dimension2 units2 space2 item2 ->
  List a
pairwiseFilterMap boundsPredicate callback set1 set2 =
  pairwiseFilterMapWithIndices boundsPredicate (\_ _ item1 item2 -> callback item1 item2) set1 set2

pairwiseFilterWithIndices ::
  (Bounds dimension1 units1 space1 -> Bounds dimension2 units2 space2 -> Bool) ->
  (Int -> Int -> item1 -> item2 -> Bool) ->
  Set dimension1 units1 space1 item1 ->
  Set dimension2 units2 space2 item2 ->
  List (item1, item2)
pairwiseFilterWithIndices boundsPredicate itemPredicate set1 set2 = do
  let callback index1 index2 item1 item2 =
        if itemPredicate index1 index2 item1 item2 then Just (item1, item2) else Nothing
  pairwiseFilterMapWithIndices boundsPredicate callback set1 set2

pairwiseFilterMapWithIndices ::
  (Bounds dimension1 units1 space1 -> Bounds dimension2 units2 space2 -> Bool) ->
  (Int -> Int -> item1 -> item2 -> Maybe a) ->
  Set dimension1 units1 space1 item1 ->
  Set dimension2 units2 space2 item2 ->
  List a
pairwiseFilterMapWithIndices boundsPredicate callback set1 set2 =
  pairwiseFilterMapWithIndicesImpl 0 0 boundsPredicate callback set1 set2 []

pairwiseFilterMapWithIndicesImpl ::
  Int ->
  Int ->
  (Bounds dimension1 units1 space1 -> Bounds dimension2 units2 space2 -> Bool) ->
  (Int -> Int -> item1 -> item2 -> Maybe a) ->
  Set dimension1 units1 space1 item1 ->
  Set dimension2 units2 space2 item2 ->
  List a ->
  List a
pairwiseFilterMapWithIndicesImpl startIndex1 startIndex2 boundsPredicate callback set1 set2 accumulated =
  if boundsPredicate (bounds set1) (bounds set2)
    then case (set1, set2) of
      (Leaf{}, Leaf{}) ->
        case callback startIndex1 startIndex2 set1.leafItem set2.leafItem of
          Just result -> result : accumulated
          Nothing -> accumulated
      (Leaf{}, Node{}) -> do
        let leftChild2 = set2.leftChild
        let rightChild2 = set2.rightChild
        let rightStartIndex2 = startIndex2 + set2.leftSize
        accumulated
          & pairwiseFilterMapWithIndicesImpl startIndex1 rightStartIndex2 boundsPredicate callback set1 rightChild2
          & pairwiseFilterMapWithIndicesImpl startIndex1 startIndex2 boundsPredicate callback set1 leftChild2
      (Node{}, Leaf{}) -> do
        let leftChild1 = set1.leftChild
        let rightChild1 = set1.rightChild
        let rightStartIndex1 = startIndex1 + set1.leftSize
        accumulated
          & pairwiseFilterMapWithIndicesImpl rightStartIndex1 startIndex2 boundsPredicate callback rightChild1 set2
          & pairwiseFilterMapWithIndicesImpl startIndex1 startIndex2 boundsPredicate callback leftChild1 set2
      (Node{}, Node{}) -> do
        let leftChild1 = set1.leftChild
        let rightChild1 = set1.rightChild
        let leftChild2 = set2.leftChild
        let rightChild2 = set2.rightChild
        let rightStartIndex1 = startIndex1 + set1.leftSize
        let rightStartIndex2 = startIndex2 + set2.leftSize
        accumulated
          & pairwiseFilterMapWithIndicesImpl rightStartIndex1 rightStartIndex2 boundsPredicate callback rightChild1 rightChild2
          & pairwiseFilterMapWithIndicesImpl rightStartIndex1 startIndex2 boundsPredicate callback rightChild1 leftChild2
          & pairwiseFilterMapWithIndicesImpl startIndex1 rightStartIndex2 boundsPredicate callback leftChild1 rightChild2
          & pairwiseFilterMapWithIndicesImpl startIndex1 startIndex2 boundsPredicate callback leftChild1 leftChild2
    else accumulated

clusters ::
  Bounds.Exists dimension units space =>
  (Bounds dimension units space -> Bounds dimension units space -> Bool) ->
  (item -> item -> Bool) ->
  Set dimension units space item ->
  Set dimension units space (Set dimension units space item)
clusters boundsPredicate itemPredicate set = do
  let callback index1 index2 item1 item2 =
        if index1 /= index2 && itemPredicate item1 item2
          then Just (index1, index2)
          else Nothing
  let edges = pairwiseFilterMapWithIndices boundsPredicate callback set set
  let graph = Graph.buildG (0, size set - 1) edges
  case Graph.components graph of
    NonEmpty components -> do
      let subsets = NonEmpty.map (buildCluster set) components
      build bounds subsets
    [] -> InternalError.throw "Should have at least one cluster (since sets cannot be empty)"

buildCluster ::
  Bounds.Exists dimension units space =>
  Set dimension units space item ->
  Graph.Tree Int ->
  Set dimension units space item
buildCluster set (Graph.Node vertex connected) =
  NonEmpty.one (getLeaf vertex set)
    & List.forEach connected (collectClusterItems set)
    & aggregate

collectClusterItems ::
  Set dimension units space item ->
  Graph.Tree Int ->
  NonEmpty (Set dimension units space item) ->
  NonEmpty (Set dimension units space item)
collectClusterItems set (Graph.Node vertex connected) accumulated =
  accumulated
    & NonEmpty.push (getLeaf vertex set)
    & List.forEach connected (collectClusterItems set)
