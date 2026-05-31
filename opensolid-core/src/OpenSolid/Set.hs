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

import Data.Foldable1 qualified
import Data.Graph qualified as Graph
import OpenSolid.Chainable (Chainable)
import OpenSolid.Chainable qualified as Chainable
import OpenSolid.IndexOutOfBounds (IndexOutOfBounds (..))
import OpenSolid.InternalError qualified as InternalError
import OpenSolid.Interval qualified as Interval
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Set.Bounds (Bounds)
import OpenSolid.Set.Bounds qualified as Set.Bounds
import OpenSolid.Units qualified as Units
import Prelude qualified

data Set b a where
  Leaf ::
    { leafBounds :: b
    , leafItem :: a
    } ->
    Set b a
  Node ::
    { nodeBounds :: b
    , leftSize :: Int
    , rightSize :: Int
    , leftChild :: Set b a
    , rightChild :: Set b a
    } ->
    Set b a

deriving instance (Show b, Show a) => Show (Set b a)

instance Foldable (Set b) where
  foldl = foldl
  foldr = foldr

instance Bounds b => Prelude.Semigroup (Set b a) where
  (<>) = union

instance
  (Units.Coercion b1 b2, Units.Coercion a1 a2) =>
  Units.Coercion (Set b1 a1) (Set b2 a2)
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

instance Indexed (Set b a) Int a where
  set !! index
    | index >= 0 && index <= size set = get index set
    | otherwise = throw IndexOutOfBounds{index = index, size = size set}

get :: Int -> Set b a -> a
get index set = case set of
  Node{leftSize, leftChild, rightChild}
    | index < leftSize -> get index leftChild
    | otherwise -> get (index - leftSize) rightChild
  Leaf{leafItem} -> assert (index == 0) leafItem

size :: Set b a -> Int
size Leaf{} = 1
size Node{leftSize, rightSize} = leftSize + rightSize

bounds :: Set b a -> b
bounds Node{nodeBounds} = nodeBounds
bounds Leaf{leafBounds} = leafBounds

singleton :: b -> a -> Set b a
singleton = Leaf

build :: Bounds b => (a -> b) -> NonEmpty a -> Set b a
build boundsFunction items = do
  let toLeaf item = Leaf (boundsFunction item) item
  aggregate (NonEmpty.map toLeaf items)

linear :: Bounds b => (a -> b) -> NonEmpty a -> Set b a
linear boundsFunction items = do
  let toLeaf item = Leaf (boundsFunction item) item
  buildLinear (NonEmpty.map toLeaf items)

buildLinear :: Bounds b => NonEmpty (Set b a) -> Set b a
buildLinear sets =
  case reduceLinear sets of
    NonEmpty.One set -> set
    reduced -> buildLinear reduced

reduceLinear :: Bounds b => NonEmpty (Set b a) -> NonEmpty (Set b a)
reduceLinear (first :| []) = NonEmpty.one first
reduceLinear (first :| second : []) = NonEmpty.one (union first second)
reduceLinear (first :| second : NonEmpty rest) =
  NonEmpty.push (union first second) (reduceLinear rest)

aggregate :: Bounds b => NonEmpty (Set b a) -> Set b a
aggregate subsets = aggregateImpl (NonEmpty.length subsets) subsets 0

aggregateImpl :: Bounds b => Int -> NonEmpty (Set b a) -> Int -> Set b a
aggregateImpl count subsets index
  | count == 1 = assert (NonEmpty.length subsets == 1) (NonEmpty.first subsets)
  | otherwise = assert (count >= 2 && NonEmpty.length subsets == count) do
      let indexedCoordinateMidpoint = Interval.midpoint . Set.Bounds.cycle index . bounds
      let sorted = NonEmpty.sortBy indexedCoordinateMidpoint subsets
      let leftSize = count // 2
      let rightSize = count - leftSize
      let (leftSubsets, rightSubsets) = splitAtIndex leftSize sorted
      let leftChild = aggregateImpl leftSize leftSubsets (index + 1)
      let rightChild = aggregateImpl rightSize rightSubsets (index + 1)
      let nodeBounds = Set.Bounds.aggregate (bounds leftChild) (bounds rightChild)
      Node{nodeBounds, leftSize, rightSize, leftChild, rightChild}

splitAtIndex :: Int -> NonEmpty a -> (NonEmpty a, NonEmpty a)
splitAtIndex 0 _ = InternalError.throw "Bad split index in Set.aggregateImpl"
splitAtIndex _ (_ :| []) = InternalError.throw "Bad split index in Set.aggregateImpl"
splitAtIndex 1 (first :| NonEmpty rest) = (NonEmpty.one first, rest)
splitAtIndex n (first :| NonEmpty rest) =
  Pair.mapFirst (NonEmpty.push first) (splitAtIndex (n - 1) rest)

flatten :: Set b (Set b a) -> Set b a
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

map :: Bounds b2 => (a1 -> a2) -> (a2 -> b2) -> Set b1 a1 -> Set b2 a2
map function boundsFunction set = mapWithIndex (const function) boundsFunction set

mapWithIndex :: Bounds b2 => (Int -> a1 -> a2) -> (a2 -> b2) -> Set b1 a1 -> Set b2 a2
mapWithIndex function boundsFunction =
  combineWithIndex \index item1 -> do
    let item2 = function index item1
    let bounds2 = boundsFunction item2
    singleton bounds2 item2

reverseMap :: Bounds b2 => (a1 -> a2) -> (a2 -> b2) -> Set b1 a1 -> Set b2 a2
reverseMap function boundsFunction set = case set of
  Leaf{leafItem} -> do
    let mappedItem = function leafItem
    let mappedBounds = boundsFunction mappedItem
    Leaf{leafBounds = mappedBounds, leafItem = mappedItem}
  Node{leftChild, rightChild} -> do
    let reverseMappedLeft = reverseMap function boundsFunction leftChild
    let reverseMappedRight = reverseMap function boundsFunction rightChild
    union reverseMappedRight reverseMappedLeft

combine :: Bounds b2 => (a1 -> Set b2 a2) -> Set b1 a1 -> Set b2 a2
combine function set = combineWithIndex (const function) set

combineWithIndex :: Bounds b2 => (Int -> a1 -> Set b2 a2) -> Set b1 a1 -> Set b2 a2
combineWithIndex function set = combineWithIndexImpl 0 function set

combineWithIndexImpl :: Bounds b2 => Int -> (Int -> a1 -> Set b2 a2) -> Set b1 a1 -> Set b2 a2
combineWithIndexImpl startIndex function set = case set of
  Leaf{leafItem} -> function startIndex leafItem
  Node{leftSize, leftChild, rightChild} -> do
    let combinedLeft = combineWithIndexImpl startIndex function leftChild
    let combinedRight = combineWithIndexImpl (startIndex + leftSize) function rightChild
    union combinedLeft combinedRight

forEach :: Chainable action => Set b a -> (a -> action) -> action
forEach set function = forEachWithIndex set (const function)

forEachWithIndex :: Chainable action => Set b a -> (Int -> a -> action) -> action
forEachWithIndex = forEachWithIndexImpl 0

forEachWithIndexImpl :: Chainable action => Int -> Set b a -> (Int -> a -> action) -> action
forEachWithIndexImpl startIndex set function = case set of
  Leaf{leafItem} -> function startIndex leafItem
  Node{leftSize, leftChild, rightChild} -> do
    let leftAction = forEachWithIndexImpl startIndex leftChild function
    let rightAction = forEachWithIndexImpl (startIndex + leftSize) rightChild function
    Chainable.chain leftAction rightAction

reverseForEach :: Chainable action => Set b a -> (a -> action) -> action
reverseForEach set function = reverseForEachWithIndex set (const function)

reverseForEachWithIndex :: Chainable action => Set b a -> (Int -> a -> action) -> action
reverseForEachWithIndex = reverseForEachWithIndexImpl 0

reverseForEachWithIndexImpl :: Chainable action => Int -> Set b a -> (Int -> a -> action) -> action
reverseForEachWithIndexImpl startIndex set function = case set of
  Leaf{leafItem} -> function startIndex leafItem
  Node{leftSize, leftChild, rightChild} -> do
    let rightAction = reverseForEachWithIndexImpl (startIndex + leftSize) rightChild function
    let leftAction = reverseForEachWithIndexImpl startIndex leftChild function
    Chainable.chain rightAction leftAction

foldr :: (a -> acc -> acc) -> acc -> Set b a -> acc
foldr function init set = foldrWithIndex (const function) init set

foldl :: (acc -> a -> acc) -> acc -> Set b a -> acc
foldl function init set = foldlWithIndex (const function) init set

foldrWithIndex :: (Int -> a -> acc -> acc) -> acc -> Set b a -> acc
foldrWithIndex function init set = reverseForEachWithIndex set function init

foldlWithIndex :: (Int -> acc -> a -> acc) -> acc -> Set b a -> acc
foldlWithIndex function init set =
  init & forEachWithIndex set \index item accumulated -> function index accumulated item

foldrMap :: (a -> acc) -> (a -> acc -> acc) -> Set b a -> acc
foldrMap init function set = foldrMapWithIndex (const init) (const function) set

foldrMapWithIndex :: (Int -> a -> acc) -> (Int -> a -> acc -> acc) -> Set b a -> acc
foldrMapWithIndex = foldrMapWithIndexStart 0

foldrMapWithIndexStart :: Int -> (Int -> a -> acc) -> (Int -> a -> acc -> acc) -> Set b a -> acc
foldrMapWithIndexStart startIndex init function set = case set of
  Leaf{leafItem} -> init startIndex leafItem
  Node{leftSize, leftChild, rightChild} ->
    foldrMapWithIndexStart (startIndex + leftSize) init function rightChild
      & foldrMapWithIndexContinue startIndex function leftChild

foldrMapWithIndexContinue :: Int -> (Int -> a -> acc -> acc) -> Set b a -> acc -> acc
foldrMapWithIndexContinue startIndex function set accumulated = case set of
  Leaf{leafItem} -> function startIndex leafItem accumulated
  Node{leftSize, leftChild, rightChild} ->
    accumulated
      & foldrMapWithIndexContinue (startIndex + leftSize) function rightChild
      & foldrMapWithIndexContinue startIndex function leftChild

foldlMap :: (a -> acc) -> (acc -> a -> acc) -> Set b a -> acc
foldlMap init function set = foldlMapWithIndex (const init) (const function) set

foldlMapWithIndex :: (Int -> a -> acc) -> (Int -> acc -> a -> acc) -> Set b a -> acc
foldlMapWithIndex init function set = foldlMapWithIndexStart 0 init function set

foldlMapWithIndexStart :: Int -> (Int -> a -> acc) -> (Int -> acc -> a -> acc) -> Set b a -> acc
foldlMapWithIndexStart startIndex init function set = case set of
  Leaf{leafItem} -> init startIndex leafItem
  Node{leftSize, leftChild, rightChild} ->
    foldlMapWithIndexStart startIndex init function leftChild
      & foldlMapWithIndexContinue (startIndex + leftSize) function rightChild

foldlMapWithIndexContinue :: Int -> (Int -> acc -> a -> acc) -> Set b a -> acc -> acc
foldlMapWithIndexContinue startIndex function set accumulated = case set of
  Leaf{leafItem} -> function startIndex accumulated leafItem
  Node{leftSize, leftChild, rightChild} ->
    accumulated
      & foldlMapWithIndexContinue startIndex function leftChild
      & foldlMapWithIndexContinue (startIndex + leftSize) function rightChild

toNonEmpty :: Set b a -> NonEmpty a
toNonEmpty = foldrMap NonEmpty.one NonEmpty.push

toNonEmptyOf :: (a1 -> a2) -> Set b a1 -> NonEmpty a2
toNonEmptyOf function = foldrMap (NonEmpty.one . function) (NonEmpty.push . function)

toNonEmptyWithIndex :: (Int -> a1 -> a2) -> Set b a1 -> NonEmpty a2
toNonEmptyWithIndex function =
  foldrMapWithIndex
    (\index item -> NonEmpty.one (function index item))
    (\index item accumulated -> NonEmpty.push (function index item) accumulated)

toList :: Set b a -> List a
toList = NonEmpty.toList . toNonEmpty

toListOf :: (a1 -> a2) -> Set b a1 -> List a2
toListOf function = NonEmpty.toList . toNonEmptyOf function

toListWithIndex :: (Int -> a1 -> a2) -> Set b a1 -> List a2
toListWithIndex function = NonEmpty.toList . toNonEmptyWithIndex function

union :: Bounds b => Set b a -> Set b a -> Set b a
union leftChild rightChild =
  Node
    { nodeBounds = Set.Bounds.aggregate (bounds leftChild) (bounds rightChild)
    , leftSize = size leftChild
    , rightSize = size rightChild
    , leftChild
    , rightChild
    }

cull :: (b -> Bool) -> Set b a -> List a
cull boundsPredicate set = filterMap boundsPredicate Just set

filter :: (b -> Bool) -> (a -> Bool) -> Set b a -> List a
filter boundsPredicate itemPredicate set =
  filterWithIndex boundsPredicate (const itemPredicate) set

filterMap :: (b -> Bool) -> (a1 -> Maybe a2) -> Set b a1 -> List a2
filterMap boundsPredicate callback set =
  filterMapWithIndex boundsPredicate (const callback) set

filterWithIndex :: (b -> Bool) -> (Int -> a -> Bool) -> Set b a -> List a
filterWithIndex boundsPredicate itemPredicate set = do
  let callback index item = if itemPredicate index item then Just item else Nothing
  filterMapWithIndex boundsPredicate callback set

filterMapWithIndex :: (b -> Bool) -> (Int -> a1 -> Maybe a2) -> Set b a1 -> List a2
filterMapWithIndex boundsPredicate callback set =
  filterMapWithIndexImpl 0 boundsPredicate callback set []

filterMapWithIndexImpl ::
  Int ->
  (b -> Bool) ->
  (Int -> a1 -> Maybe a2) ->
  Set b a1 ->
  List a2 ->
  List a2
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

subset :: Bounds b => (b -> Bool) -> (a -> Bool) -> Set b a -> Maybe (Set b a)
subset boundsPredicate itemPredicate set = case set of
  Leaf{leafBounds, leafItem} ->
    if boundsPredicate leafBounds && itemPredicate leafItem then Just set else Nothing
  Node{nodeBounds, leftChild, rightChild} ->
    if boundsPredicate nodeBounds
      then do
        let maybeLeft = subset boundsPredicate itemPredicate leftChild
        let maybeRight = subset boundsPredicate itemPredicate rightChild
        maybeLeft <> maybeRight
      else Nothing

any :: (b -> Bool) -> (a -> Bool) -> Set b a -> Bool
any boundsPredicate itemPredicate set = case set of
  Leaf{leafBounds, leafItem} -> boundsPredicate leafBounds && itemPredicate leafItem
  Node{nodeBounds, leftChild, rightChild} ->
    boundsPredicate nodeBounds && do
      any boundsPredicate itemPredicate leftChild || any boundsPredicate itemPredicate rightChild

all :: (b -> Bool) -> (a -> Bool) -> Set b a -> Bool
all boundsPredicate itemPredicate set = case set of
  Leaf{leafBounds, leafItem} -> boundsPredicate leafBounds && itemPredicate leafItem
  Node{nodeBounds, leftChild, rightChild} ->
    boundsPredicate nodeBounds && do
      all boundsPredicate itemPredicate leftChild && all boundsPredicate itemPredicate rightChild

pairwiseFilter ::
  (b1 -> b2 -> Bool) ->
  (a1 -> a2 -> Bool) ->
  Set b1 a1 ->
  Set b2 a2 ->
  List (a1, a2)
pairwiseFilter boundsPredicate itemPredicate set1 set2 = do
  let callback item1 item2 = if itemPredicate item1 item2 then Just (item1, item2) else Nothing
  pairwiseFilterMap boundsPredicate callback set1 set2

pairwiseFilterMap ::
  (b1 -> b2 -> Bool) ->
  (a1 -> a2 -> Maybe a3) ->
  Set b1 a1 ->
  Set b2 a2 ->
  List a3
pairwiseFilterMap boundsPredicate callback set1 set2 =
  pairwiseFilterMapWithIndices boundsPredicate (\_ _ item1 item2 -> callback item1 item2) set1 set2

pairwiseFilterWithIndices ::
  (b1 -> b2 -> Bool) ->
  (Int -> Int -> a1 -> a2 -> Bool) ->
  Set b1 a1 ->
  Set b2 a2 ->
  List (a1, a2)
pairwiseFilterWithIndices boundsPredicate itemPredicate set1 set2 = do
  let callback index1 index2 item1 item2 =
        if itemPredicate index1 index2 item1 item2 then Just (item1, item2) else Nothing
  pairwiseFilterMapWithIndices boundsPredicate callback set1 set2

pairwiseFilterMapWithIndices ::
  (b1 -> b2 -> Bool) ->
  (Int -> Int -> a1 -> a2 -> Maybe a3) ->
  Set b1 a1 ->
  Set b2 a2 ->
  List a3
pairwiseFilterMapWithIndices boundsPredicate callback set1 set2 =
  pairwiseFilterMapWithIndicesImpl 0 0 boundsPredicate callback set1 set2 []

pairwiseFilterMapWithIndicesImpl ::
  Int ->
  Int ->
  (b1 -> b2 -> Bool) ->
  (Int -> Int -> a1 -> a2 -> Maybe a3) ->
  Set b1 a1 ->
  Set b2 a2 ->
  List a3 ->
  List a3
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

clusters :: Bounds b => (b -> b -> Bool) -> (a -> a -> Bool) -> Set b a -> NonEmpty (NonEmpty a)
clusters boundsPredicate itemPredicate set = do
  let callback index1 index2 item1 item2 =
        if index1 /= index2 && itemPredicate item1 item2
          then Just (index1, index2)
          else Nothing
  let edges = pairwiseFilterMapWithIndices boundsPredicate callback set set
  let graph = Graph.buildG (0, size set - 1) edges
  case Graph.components graph of
    NonEmpty components -> NonEmpty.map (buildCluster set) components
    [] -> InternalError.throw "Should have at least one cluster (since sets cannot be empty)"

buildCluster :: Bounds b => Set b a -> Graph.Tree Int -> NonEmpty a
buildCluster set tree = NonEmpty.map (set !!) (Data.Foldable1.toNonEmpty tree)
