{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Solve1d
  ( Subdomain
  , domain
  , isAtomic
  , bisect
  , half
  , interior
  , bounds
  , overlaps
  , contains
  , isResolved
  , resolvedSign
  , Neighborhood
  , neighborhood
  , derivativeTolerance
  , Cache
  , init
  , SomeExclusions
  , NoExclusions
  , Exclusions (NoExclusions, SomeExclusions)
  , InfiniteRecursion (InfiniteRecursion)
  , run
  , Action
  , return
  , recurse
  , pass
  )
where

import Float qualified
import Int qualified
import List qualified
import OpenSolid
import Qty qualified
import Queue (Queue)
import Queue qualified
import Range (Range)
import Range qualified

data Subdomain = Subdomain
  { n :: Float
  , i :: Float
  , j :: Float
  }
  deriving (Eq, Show)

domain :: Subdomain
domain = Subdomain{n = 1.0, i = 0.0, j = 1.0}

isAtomic :: Subdomain -> Bool
isAtomic (Subdomain{n, i, j}) = (j - i) / n <= Float.epsilon

bisect :: Subdomain -> (Subdomain, Subdomain)
bisect (Subdomain{n, i, j}) = do
  let n2 = 2 * n
  let i2 = 2 * i
  let j2 = 2 * j
  let mid = i2 + (j - i)
  (Subdomain n2 i2 mid, Subdomain n2 mid j2)

half :: Subdomain -> Subdomain
half (Subdomain{n, i, j}) = do
  let delta = j - i
  Subdomain (4 * n) (4 * i + delta) (4 * j - delta)

bounds :: Subdomain -> Range Unitless
bounds (Subdomain{n, i, j}) = Range.unsafe (i / n) (j / n)

interior :: Subdomain -> Range Unitless
interior (Subdomain{n, i, j}) = do
  let n8 = 8 * n
  let delta = j - i
  Range.unsafe
    (if i == 0.0 then 0.0 else (8 * i + delta) / n8)
    (if j == n then 1.0 else (8 * j - delta) / n8)

overlaps :: Subdomain -> Subdomain -> Bool
overlaps (Subdomain n2 i2 j2) (Subdomain n1 i1 j1) =
  i1 * n2 < j2 * n1 && j1 * n2 > i2 * n1

contains :: Subdomain -> Subdomain -> Bool
contains (Subdomain n2 i2 j2) (Subdomain n1 i1 j1) =
  i1 * n2 <= i2 * n1 && j1 * n2 >= j2 * n1

isResolved :: Range units -> Bool
isResolved range = resolvedSign range /= Nothing

resolvedSign :: Range units -> Maybe Sign
resolvedSign range = do
  let resolution = Range.resolution range
  if Qty.abs resolution >= 0.5 then Just (Qty.sign resolution) else Nothing

data Neighborhood units = Neighborhood {n :: Int, derivativeMagnitude :: Qty units, radius :: Float}

neighborhood :: Tolerance units => Int -> Qty units -> Neighborhood units
neighborhood n derivativeMagnitude = do
  let radius = (Int.factorial n * ?tolerance / derivativeMagnitude) ** (1 / n)
  Neighborhood{n, derivativeMagnitude, radius}

derivativeTolerance :: Neighborhood units -> Int -> Qty units
derivativeTolerance (Neighborhood{n, derivativeMagnitude, radius}) k =
  derivativeMagnitude * radius ** (n - k) / Int.factorial (n - k)

data Cache cached
  = Tree Subdomain cached (Node cached)

data Node cached
  = Atomic
  | Splittable ~(Cache cached) ~(Cache cached) ~(Cache cached)
  | Shrinkable ~(Cache cached)

init :: (Range Unitless -> cached) -> Cache cached
init function = split function domain

tree :: (Range Unitless -> cached) -> Subdomain -> Node cached -> Cache cached
tree function subdomain givenNode = do
  let cached = function (bounds subdomain)
  let node = if isAtomic subdomain then Atomic else givenNode
  Tree subdomain cached node

split :: (Range Unitless -> cached) -> Subdomain -> Cache cached
split function subdomain = do
  let middleSubdomain = half subdomain
  let (leftSubdomain, rightSubdomain) = bisect subdomain
  let middleChild = shrink function middleSubdomain
  let leftChild = split function leftSubdomain
  let rightChild = split function rightSubdomain
  tree function subdomain (Splittable middleChild leftChild rightChild)

shrink :: (Range Unitless -> cached) -> Subdomain -> Cache cached
shrink function subdomain = do
  let child = shrink function (half subdomain)
  tree function subdomain (Shrinkable child)

data NoExclusions

data SomeExclusions

data Exclusions exclusions where
  NoExclusions :: Exclusions NoExclusions
  SomeExclusions :: NonEmpty Subdomain -> Exclusions SomeExclusions

data InfiniteRecursion = InfiniteRecursion deriving (Eq, Show, Error)

type Callback cached solution =
  forall exclusions.
  Subdomain ->
  cached ->
  Exclusions exclusions ->
  Action exclusions solution

run ::
  Callback cached solution ->
  Cache cached ->
  List solution ->
  List Subdomain ->
  Result InfiniteRecursion (List solution, List Subdomain)
run callback cache solutions exclusions =
  process callback (Queue.singleton cache) solutions exclusions

process ::
  forall cached solution.
  Callback cached solution ->
  Queue (Cache cached) ->
  List solution ->
  List Subdomain ->
  Result InfiniteRecursion (List solution, List Subdomain)
process callback queue solutions exclusions =
  case Queue.pop queue of
    Just (Tree subdomain cached node, remaining) -> do
      let filteredExclusions = List.filter (overlaps subdomain) exclusions
      if List.any (contains subdomain) filteredExclusions
        then process callback remaining solutions exclusions
        else case filteredExclusions of
          NonEmpty someExclusions ->
            case callback subdomain cached (SomeExclusions someExclusions) of
              Pass -> process callback remaining solutions exclusions
              Recurse -> recurseIntoChildrenOf node callback remaining solutions exclusions
          [] ->
            case callback subdomain cached NoExclusions of
              Pass -> process callback remaining solutions exclusions
              Recurse -> recurseIntoChildrenOf node callback remaining solutions exclusions
              Return solution ->
                process callback remaining (solution : solutions) (subdomain : exclusions)
    Nothing -> Ok (solutions, exclusions)

{-# INLINE recurseIntoChildrenOf #-}
recurseIntoChildrenOf ::
  forall cached solution.
  Node cached ->
  Callback cached solution ->
  Queue (Cache cached) ->
  List solution ->
  List Subdomain ->
  Result InfiniteRecursion (List solution, List Subdomain)
recurseIntoChildrenOf node callback queue solutions exclusions =
  case node of
    Atomic -> Error InfiniteRecursion
    Shrinkable child -> process callback (queue + child) solutions exclusions
    Splittable middleChild leftChild rightChild -> do
      let updatedQueue = queue + middleChild + leftChild + rightChild
      process callback updatedQueue solutions exclusions

data Action exclusions solution where
  Return :: solution -> Action NoExclusions solution
  Recurse :: Action exclusions solution
  Pass :: Action exclusions solution

return :: solution -> Action NoExclusions solution
return = Return

recurse :: Action exclusions solution
recurse = Recurse

pass :: Action exclusions solution
pass = Pass
