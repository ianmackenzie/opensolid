module OpenSolid.Set
  ( Set (..)
  , size
  , bounds
  , leaf
  , node
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
import Data.Proxy (Proxy (Proxy))
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
  Leaf :: {leafBounds :: b, leafItem :: a} -> Set b a
  Node :: {nodeBounds :: b, nodeSize :: Int, children :: NonEmpty (Set b a)} -> Set b a

deriving instance (Show b, Show a) => Show (Set b a)

instance Foldable (Set b) where
  foldl = foldl
  foldr = foldr

instance Bounds b => Prelude.Semigroup (Set b a) where
  (<>) = node2

instance
  (Units.Coercion b1 b2, Units.Coercion a1 a2) =>
  Units.Coercion (Set b1 a1) (Set b2 a2)
  where
  coerce Leaf{leafBounds, leafItem} =
    Leaf
      { leafBounds = Units.coerce leafBounds
      , leafItem = Units.coerce leafItem
      }
  coerce Node{nodeBounds, nodeSize, children} =
    Node
      { nodeBounds = Units.coerce nodeBounds
      , nodeSize
      , children = NonEmpty.map Units.coerce children
      }

instance Indexed (Set b a) Int a where
  set !! index =
    case get index set of
      Just item -> item
      Nothing -> throw IndexOutOfBounds{index = index, size = size set}

get :: Int -> Set b a -> Maybe a
get index set = case set of
  Node{children} -> getInChildren index children
  Leaf{leafItem} -> if index == 0 then Just leafItem else Nothing

getInChildren :: Int -> NonEmpty (Set b a) -> Maybe a
getInChildren index (first :| rest) = do
  let firstSize = size first
  if index < firstSize
    then get index first
    else case rest of
      NonEmpty remaining -> getInChildren (index - firstSize) remaining
      [] -> Nothing

size :: Set b a -> Int
size Leaf{} = 1
size Node{nodeSize} = nodeSize

bounds :: Set b a -> b
bounds Node{nodeBounds} = nodeBounds
bounds Leaf{leafBounds} = leafBounds

leaf :: b -> a -> Set b a
leaf = Leaf

node :: Bounds b => NonEmpty (Set b a) -> Set b a
node (NonEmpty.One child) = child
node children =
  Node
    { nodeBounds = Set.Bounds.aggregateOf bounds children
    , nodeSize = NonEmpty.sumOf size children
    , children
    }

node2 :: Bounds b => Set b a -> Set b a -> Set b a
node2 left right = node (NonEmpty.two left right)

build :: Bounds b => (a -> b) -> NonEmpty a -> Set b a
build boundsFunction items = do
  let toLeaf item = leaf (boundsFunction item) item
  aggregate (NonEmpty.map toLeaf items)

linear :: Bounds b => (a -> b) -> NonEmpty a -> Set b a
linear boundsFunction items = do
  let toLeaf item = leaf (boundsFunction item) item
  buildLinear (NonEmpty.map toLeaf items)

buildLinear :: Bounds b => NonEmpty (Set b a) -> Set b a
buildLinear sets =
  case reduceLinear sets of
    NonEmpty.One set -> set
    reduced -> buildLinear reduced

reduceLinear :: Bounds b => NonEmpty (Set b a) -> NonEmpty (Set b a)
reduceLinear (first :| []) = NonEmpty.one first
reduceLinear (first :| second : []) = NonEmpty.one (node2 first second)
reduceLinear (first :| second : NonEmpty rest) =
  NonEmpty.push (node2 first second) (reduceLinear rest)

aggregate :: forall b a. Bounds b => NonEmpty (Set b a) -> Set b a
aggregate subsets = do
  let dimension = Set.Bounds.dimension @b Proxy
  let count = NonEmpty.length subsets
  node (nSect dimension count subsets 0)

{-| Divide a list of subsets into subgroups as appropriate:

    - bisect into two subgroups in 1D
    - quadrisect into four subgroups in 2D
    - "octasect" into eight subgroups in 3D, etc.

So in general "n-sect" the list, where n = 2 ^ dimension
(2 ^ 1 = 2 in 1D, 2 ^ 2 = 4 in 2D, 2 ^ 3 = 8 in 3D, etc.)
-}
nSect :: Bounds b => Int -> Int -> NonEmpty (Set b a) -> Int -> NonEmpty (Set b a)
nSect dimension count subsets dimensionIndex
  | count == 1 = assert (NonEmpty.length subsets == 1) subsets -- Can't split a one-element list
  | otherwise = assert (count >= 2 && NonEmpty.length subsets == count) do
      -- Split the given list into two sublists based on sorting by the current dimension index
      let indexedComponentMidpoint =
            Interval.midpoint . Set.Bounds.component dimensionIndex . bounds
      let sorted = NonEmpty.sortBy indexedComponentMidpoint subsets
      let leftSize = count // 2
      let rightSize = count - leftSize
      let (leftSubsets, rightSubsets) = splitAtIndex leftSize sorted
      if dimensionIndex == dimension - 1
        then do
          -- We've split on the final dimension index,
          -- so restart the process at dimension index 0 within each sublist
          -- by calling the top-level 'aggregate' function
          let leftNode = aggregate leftSubsets
          let rightNode = aggregate rightSubsets
          NonEmpty.two leftNode rightNode
        else do
          -- Further split each sublist based on the remaining dimension indices
          -- (e.g. after splitting based on X, recursively split sublists based on Y and then Z)
          let leftSubgroups = nSect dimension leftSize leftSubsets (dimensionIndex + 1)
          let rightSubgroups = nSect dimension rightSize rightSubsets (dimensionIndex + 1)
          leftSubgroups <> rightSubgroups

splitAtIndex :: Int -> NonEmpty a -> (NonEmpty a, NonEmpty a)
splitAtIndex 0 _ = InternalError.throw "Bad split index in Set.nSect"
splitAtIndex _ (_ :| []) = InternalError.throw "Bad split index in Set.nSect"
splitAtIndex 1 (first :| NonEmpty rest) = (NonEmpty.one first, rest)
splitAtIndex n (first :| NonEmpty rest) =
  Pair.mapFirst (NonEmpty.push first) (splitAtIndex (n - 1) rest)

flatten :: Set b (Set b a) -> Set b a
flatten Leaf{leafItem} = leafItem
flatten Node{nodeBounds, children} = do
  let flattenedChildren = NonEmpty.map flatten children
  Node
    { nodeBounds -- Flattening subsets shouldn't change the overall bounds, so no need to recompute
    , nodeSize = NonEmpty.sumOf size flattenedChildren
    , children = flattenedChildren
    }

map :: Bounds b2 => (a1 -> a2) -> (a2 -> b2) -> Set b1 a1 -> Set b2 a2
map function boundsFunction set = mapWithIndex (const function) boundsFunction set

mapWithIndex :: Bounds b2 => (Int -> a1 -> a2) -> (a2 -> b2) -> Set b1 a1 -> Set b2 a2
mapWithIndex function boundsFunction =
  combineWithIndex \index item1 -> do
    let item2 = function index item1
    let bounds2 = boundsFunction item2
    leaf bounds2 item2

reverseMap :: Bounds b2 => (a1 -> a2) -> (a2 -> b2) -> Set b1 a1 -> Set b2 a2
reverseMap function boundsFunction set = case set of
  Leaf{leafItem} -> do
    let mappedItem = function leafItem
    let mappedBounds = boundsFunction mappedItem
    Leaf{leafBounds = mappedBounds, leafItem = mappedItem}
  Node{nodeSize, children} -> do
    let reverseMappedChildren = NonEmpty.reverseMap (reverseMap function boundsFunction) children
    Node
      { nodeBounds = Set.Bounds.aggregateOf bounds reverseMappedChildren
      , nodeSize
      , children = reverseMappedChildren
      }

combine :: Bounds b2 => (a1 -> Set b2 a2) -> Set b1 a1 -> Set b2 a2
combine function set = combineWithIndex (const function) set

combineWithIndex :: Bounds b2 => (Int -> a1 -> Set b2 a2) -> Set b1 a1 -> Set b2 a2
combineWithIndex function set = combineWithIndexImpl 0 function set

combineWithIndexImpl :: Bounds b2 => Int -> (Int -> a1 -> Set b2 a2) -> Set b1 a1 -> Set b2 a2
combineWithIndexImpl startIndex function set = case set of
  Leaf{leafItem} -> function startIndex leafItem
  Node{children} -> node (combineChildrenWithIndex startIndex function children)

combineChildrenWithIndex ::
  Bounds b2 =>
  Int ->
  (Int -> a1 -> Set b2 a2) ->
  NonEmpty (Set b1 a1) ->
  NonEmpty (Set b2 a2)
combineChildrenWithIndex startIndex function children = case children of
  child :| [] -> NonEmpty.one (combineWithIndexImpl startIndex function child)
  first :| NonEmpty remaining ->
    NonEmpty.push
      (combineWithIndexImpl startIndex function first)
      (combineChildrenWithIndex (startIndex + size first) function remaining)

forEach :: Chainable action => Set b a -> (a -> action) -> action
forEach set function = forEachWithIndex set (const function)

forEachWithIndex :: Chainable action => Set b a -> (Int -> a -> action) -> action
forEachWithIndex = forEachWithIndexImpl 0

forEachWithIndexImpl :: Chainable action => Int -> Set b a -> (Int -> a -> action) -> action
forEachWithIndexImpl startIndex set function = case set of
  Leaf{leafItem} -> function startIndex leafItem
  Node{children} -> forEachChildWithIndex startIndex children function

forEachChildWithIndex ::
  Chainable action =>
  Int ->
  NonEmpty (Set b a) ->
  (Int -> a -> action) ->
  action
forEachChildWithIndex startIndex children function = case children of
  child :| [] -> forEachWithIndexImpl startIndex child function
  first :| NonEmpty remaining -> do
    let firstAction = forEachWithIndexImpl startIndex first function
    let remainingAction = forEachChildWithIndex (startIndex + size first) remaining function
    Chainable.chain firstAction remainingAction

reverseForEach :: Chainable action => Set b a -> (a -> action) -> action
reverseForEach set function = reverseForEachWithIndex set (const function)

reverseForEachWithIndex :: Chainable action => Set b a -> (Int -> a -> action) -> action
reverseForEachWithIndex = reverseForEachWithIndexImpl 0

reverseForEachWithIndexImpl :: Chainable action => Int -> Set b a -> (Int -> a -> action) -> action
reverseForEachWithIndexImpl startIndex set function = case set of
  Leaf{leafItem} -> function startIndex leafItem
  Node{children} -> reverseForEachChildWithIndex startIndex children function

reverseForEachChildWithIndex ::
  Chainable action =>
  Int ->
  NonEmpty (Set b a) ->
  (Int -> a -> action) ->
  action
reverseForEachChildWithIndex startIndex children function = case children of
  child :| [] -> forEachWithIndexImpl startIndex child function
  first :| NonEmpty remaining -> do
    let remainingAction = reverseForEachChildWithIndex (startIndex + size first) remaining function
    let firstAction = reverseForEachWithIndexImpl startIndex first function
    Chainable.chain remainingAction firstAction

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
  Node{children} -> foldrMapChildrenWithIndexStart startIndex init function children

foldrMapChildrenWithIndexStart ::
  Int ->
  (Int -> a -> acc) ->
  (Int -> a -> acc -> acc) ->
  NonEmpty (Set b a) ->
  acc
foldrMapChildrenWithIndexStart startIndex init function children = case children of
  child :| [] -> foldrMapWithIndexStart startIndex init function child
  first :| NonEmpty rest ->
    foldrMapChildrenWithIndexStart (startIndex + size first) init function rest
      & foldrMapWithIndexContinue startIndex function first

foldrMapWithIndexContinue :: Int -> (Int -> a -> acc -> acc) -> Set b a -> acc -> acc
foldrMapWithIndexContinue startIndex function set accumulated = case set of
  Leaf{leafItem} -> function startIndex leafItem accumulated
  Node{children} -> foldrMapChildrenWithIndexContinue startIndex function children accumulated

foldrMapChildrenWithIndexContinue ::
  Int ->
  (Int -> a -> acc -> acc) ->
  NonEmpty (Set b a) ->
  acc ->
  acc
foldrMapChildrenWithIndexContinue startIndex function children accumulated = case children of
  child :| [] -> foldrMapWithIndexContinue startIndex function child accumulated
  first :| NonEmpty rest ->
    accumulated
      & foldrMapChildrenWithIndexContinue (startIndex + size first) function rest
      & foldrMapWithIndexContinue startIndex function first

foldlMap :: (a -> acc) -> (acc -> a -> acc) -> Set b a -> acc
foldlMap init function set = foldlMapWithIndex (const init) (const function) set

foldlMapWithIndex :: (Int -> a -> acc) -> (Int -> acc -> a -> acc) -> Set b a -> acc
foldlMapWithIndex init function set = foldlMapWithIndexStart 0 init function set

foldlMapWithIndexStart :: Int -> (Int -> a -> acc) -> (Int -> acc -> a -> acc) -> Set b a -> acc
foldlMapWithIndexStart startIndex init function set = case set of
  Leaf{leafItem} -> init startIndex leafItem
  Node{children} -> foldlMapChildrenWithIndexStart startIndex init function children

foldlMapChildrenWithIndexStart ::
  Int ->
  (Int -> a -> acc) ->
  (Int -> acc -> a -> acc) ->
  NonEmpty (Set b a) ->
  acc
foldlMapChildrenWithIndexStart startIndex init function children = case children of
  child :| [] -> foldlMapWithIndexStart startIndex init function child
  first :| NonEmpty rest ->
    foldlMapWithIndexStart startIndex init function first
      & foldlMapChildrenWithIndexContinue (startIndex + size first) function rest

foldlMapWithIndexContinue :: Int -> (Int -> acc -> a -> acc) -> Set b a -> acc -> acc
foldlMapWithIndexContinue startIndex function set accumulated = case set of
  Leaf{leafItem} -> function startIndex accumulated leafItem
  Node{children} -> foldlMapChildrenWithIndexContinue startIndex function children accumulated

foldlMapChildrenWithIndexContinue ::
  Int ->
  (Int -> acc -> a -> acc) ->
  NonEmpty (Set b a) ->
  acc ->
  acc
foldlMapChildrenWithIndexContinue startIndex function children accumulated = case children of
  child :| [] -> foldlMapWithIndexContinue startIndex function child accumulated
  first :| NonEmpty rest ->
    accumulated
      & foldlMapWithIndexContinue startIndex function first
      & foldlMapChildrenWithIndexContinue (startIndex + size first) function rest

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
  Node{nodeBounds, children} ->
    if boundsPredicate nodeBounds
      then filterMapChildrenWithIndex startIndex boundsPredicate callback children accumulated
      else accumulated

filterMapChildrenWithIndex ::
  Int ->
  (b -> Bool) ->
  (Int -> a1 -> Maybe a2) ->
  NonEmpty (Set b a1) ->
  List a2 ->
  List a2
filterMapChildrenWithIndex startIndex boundsPredicate callback children accumulated =
  case children of
    child :| [] -> filterMapWithIndexImpl startIndex boundsPredicate callback child accumulated
    first :| NonEmpty rest ->
      accumulated
        & filterMapChildrenWithIndex (startIndex + size first) boundsPredicate callback rest
        & filterMapWithIndexImpl startIndex boundsPredicate callback first

subset :: Bounds b => (b -> Bool) -> (a -> Bool) -> Set b a -> Maybe (Set b a)
subset boundsPredicate itemPredicate set = case set of
  Leaf{leafBounds, leafItem} ->
    if boundsPredicate leafBounds && itemPredicate leafItem then Just set else Nothing
  Node{nodeBounds, children} ->
    if boundsPredicate nodeBounds
      then case NonEmpty.filterMap (subset boundsPredicate itemPredicate) children of
        NonEmpty filteredChildren -> Just (node filteredChildren)
        [] -> Nothing
      else Nothing

any :: (b -> Bool) -> (a -> Bool) -> Set b a -> Bool
any boundsPredicate itemPredicate set = case set of
  Leaf{leafBounds, leafItem} ->
    boundsPredicate leafBounds && itemPredicate leafItem
  Node{nodeBounds, children} ->
    boundsPredicate nodeBounds && NonEmpty.any (any boundsPredicate itemPredicate) children

all :: (b -> Bool) -> (a -> Bool) -> Set b a -> Bool
all boundsPredicate itemPredicate set = case set of
  Leaf{leafBounds, leafItem} ->
    boundsPredicate leafBounds && itemPredicate leafItem
  Node{nodeBounds, children} ->
    boundsPredicate nodeBounds && NonEmpty.all (all boundsPredicate itemPredicate) children

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
  pairwiseFilterMapWithIndices11 0 0 boundsPredicate callback set1 set2 []

pairwiseFilterMapWithIndices11 ::
  Int ->
  Int ->
  (b1 -> b2 -> Bool) ->
  (Int -> Int -> a1 -> a2 -> Maybe a3) ->
  Set b1 a1 ->
  Set b2 a2 ->
  List a3 ->
  List a3
pairwiseFilterMapWithIndices11 startIndex1 startIndex2 boundsPredicate callback set1 set2 accumulated =
  if not (boundsPredicate (bounds set1) (bounds set2))
    then accumulated
    else case (set1, set2) of
      (Leaf{}, Leaf{}) ->
        case callback startIndex1 startIndex2 set1.leafItem set2.leafItem of
          Just result -> result : accumulated
          Nothing -> accumulated
      (Leaf{}, Node{}) ->
        pairwiseFilterMapWithIndices1N
          startIndex1
          startIndex2
          boundsPredicate
          callback
          set1
          set2.children
          accumulated
      (Node{}, Leaf{}) ->
        pairwiseFilterMapWithIndicesN1
          startIndex1
          startIndex2
          boundsPredicate
          callback
          set1.children
          set2
          accumulated
      (Node{}, Node{}) ->
        pairwiseFilterMapWithIndicesNN
          startIndex1
          startIndex2
          boundsPredicate
          callback
          set1.children
          set2.children
          accumulated

pairwiseFilterMapWithIndices1N ::
  Int ->
  Int ->
  (b1 -> b2 -> Bool) ->
  (Int -> Int -> a1 -> a2 -> Maybe a3) ->
  Set b1 a1 ->
  NonEmpty (Set b2 a2) ->
  List a3 ->
  List a3
pairwiseFilterMapWithIndices1N startIndex1 startIndex2 boundsPredicate callback set1 sets2 accumulated =
  case sets2 of
    set2 :| [] ->
      accumulated
        & pairwiseFilterMapWithIndices11
          startIndex1
          startIndex2
          boundsPredicate
          callback
          set1
          set2
    first2 :| NonEmpty rest2 ->
      accumulated
        & pairwiseFilterMapWithIndices1N
          startIndex1
          (startIndex2 + size first2)
          boundsPredicate
          callback
          set1
          rest2
        & pairwiseFilterMapWithIndices11
          startIndex1
          startIndex2
          boundsPredicate
          callback
          set1
          first2

pairwiseFilterMapWithIndicesN1 ::
  Int ->
  Int ->
  (b1 -> b2 -> Bool) ->
  (Int -> Int -> a1 -> a2 -> Maybe a3) ->
  NonEmpty (Set b1 a1) ->
  Set b2 a2 ->
  List a3 ->
  List a3
pairwiseFilterMapWithIndicesN1 startIndex1 startIndex2 boundsPredicate callback sets1 set2 accumulated =
  case sets1 of
    set1 :| [] ->
      accumulated
        & pairwiseFilterMapWithIndices11
          startIndex1
          startIndex2
          boundsPredicate
          callback
          set1
          set2
    first1 :| NonEmpty rest1 ->
      accumulated
        & pairwiseFilterMapWithIndicesN1
          (startIndex1 + size first1)
          startIndex2
          boundsPredicate
          callback
          rest1
          set2
        & pairwiseFilterMapWithIndices11
          startIndex1
          startIndex2
          boundsPredicate
          callback
          first1
          set2

pairwiseFilterMapWithIndicesNN ::
  Int ->
  Int ->
  (b1 -> b2 -> Bool) ->
  (Int -> Int -> a1 -> a2 -> Maybe a3) ->
  NonEmpty (Set b1 a1) ->
  NonEmpty (Set b2 a2) ->
  List a3 ->
  List a3
pairwiseFilterMapWithIndicesNN startIndex1 startIndex2 boundsPredicate callback sets1 sets2 accumulated =
  case sets1 of
    set1 :| [] ->
      accumulated
        & pairwiseFilterMapWithIndices1N
          startIndex1
          startIndex2
          boundsPredicate
          callback
          set1
          sets2
    first1 :| NonEmpty rest1 ->
      accumulated
        & pairwiseFilterMapWithIndicesNN
          (startIndex1 + size first1)
          startIndex2
          boundsPredicate
          callback
          rest1
          sets2
        & pairwiseFilterMapWithIndices1N
          startIndex1
          startIndex2
          boundsPredicate
          callback
          first1
          sets2

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
