{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Solve1d
  ( Neighborhood
  , neighborhood
  , derivativeTolerance
  , root
  , Cache
  , init
  , SomeExclusions
  , NoExclusions
  , Exclusions (NoExclusions, SomeExclusions)
  , InfiniteRecursion (InfiniteRecursion)
  , search
  , Action
  , return
  , recurse
  , pass
  , monotonic
  )
where

import Curve1d.Root (Root (Root))
import Domain1d (Domain1d)
import Domain1d qualified
import Error qualified
import Int qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Pair qualified
import Qty qualified
import Queue (Queue)
import Queue qualified
import Range (Range (Range))
import Range qualified
import Result qualified

data Neighborhood units = Neighborhood
  { n :: Int
  , sign :: Sign
  , magnitude :: Qty units
  , radius :: Float
  }
  deriving (Show)

neighborhood :: Tolerance units => Int -> Qty units -> Neighborhood units
neighborhood n value = do
  let sign = Qty.sign value
  let magnitude = Qty.abs value
  let radius = (Int.factorial n * ?tolerance / magnitude) ** (1 / n)
  Neighborhood{n, sign, magnitude, radius}

derivativeTolerance :: Neighborhood units -> Int -> Qty units
derivativeTolerance (Neighborhood{n, magnitude, radius}) k = do
  magnitude * radius ** (n - k) / Int.factorial (n - k)

root :: Float -> Neighborhood units -> Root
root value (Neighborhood{n, sign}) = Root value (n - 1) sign

data Cache cached
  = Tree Domain1d cached (Node cached)

data Node cached
  = Atomic
  | Splittable ~(Cache cached) ~(Cache cached) ~(Cache cached)
  | Shrinkable ~(Cache cached)

init :: (Range Unitless -> cached) -> Cache cached
init function = split function Domain1d.unit

tree :: (Range Unitless -> cached) -> Domain1d -> Node cached -> Cache cached
tree function subdomain givenNode = do
  let cached = function (Domain1d.bounds subdomain)
  let node = if Domain1d.isAtomic subdomain then Atomic else givenNode
  Tree subdomain cached node

split :: (Range Unitless -> cached) -> Domain1d -> Cache cached
split function subdomain = do
  let middleSubdomain = Domain1d.half subdomain
  let (leftSubdomain, rightSubdomain) = Domain1d.bisect subdomain
  let middleChild = shrink function middleSubdomain
  let leftChild = split function leftSubdomain
  let rightChild = split function rightSubdomain
  tree function subdomain (Splittable middleChild leftChild rightChild)

shrink :: (Range Unitless -> cached) -> Domain1d -> Cache cached
shrink function subdomain = do
  let child = shrink function (Domain1d.half subdomain)
  tree function subdomain (Shrinkable child)

data NoExclusions

data SomeExclusions

data Exclusions exclusions where
  NoExclusions :: Exclusions NoExclusions
  SomeExclusions :: Exclusions SomeExclusions

data InfiniteRecursion = InfiniteRecursion deriving (Eq, Show, Error.Message)

type Callback cached solution =
  forall exclusions.
  Domain1d ->
  cached ->
  Exclusions exclusions ->
  Action exclusions solution

search ::
  Callback cached solution ->
  Cache cached ->
  Result InfiniteRecursion (List solution)
search callback cache =
  Result.map Pair.first $
    process callback (Queue.singleton cache) [] []

process ::
  Callback cached solution ->
  Queue (Cache cached) ->
  List solution ->
  List Domain1d ->
  Result InfiniteRecursion (List solution, List Domain1d)
process callback queue solutions exclusions =
  case Queue.pop queue of
    Just (Tree subdomain cached node, remaining) -> do
      let filteredExclusions = List.filter (Domain1d.overlaps subdomain) exclusions
      if List.anySatisfy (Domain1d.contains subdomain) filteredExclusions
        then process callback remaining solutions exclusions
        else case filteredExclusions of
          [] -> case callback subdomain cached NoExclusions of
            Pass -> process callback remaining solutions exclusions
            Recurse -> recurseIntoChildrenOf node callback remaining solutions exclusions
            Return newSolutions -> do
              let updatedSolutions = NonEmpty.toList newSolutions + solutions
              let updatedExclusions = subdomain : exclusions
              process callback remaining updatedSolutions updatedExclusions
          List.OneOrMore -> case callback subdomain cached SomeExclusions of
            Pass -> process callback remaining solutions exclusions
            Recurse -> recurseIntoChildrenOf node callback remaining solutions exclusions
    Nothing -> Success (solutions, exclusions)

{-# INLINE recurseIntoChildrenOf #-}
recurseIntoChildrenOf ::
  Node cached ->
  Callback cached solution ->
  Queue (Cache cached) ->
  List solution ->
  List Domain1d ->
  Result InfiniteRecursion (List solution, List Domain1d)
recurseIntoChildrenOf node callback queue solutions exclusions =
  case node of
    Atomic -> Failure InfiniteRecursion
    Shrinkable child -> process callback (queue + child) solutions exclusions
    Splittable middleChild leftChild rightChild -> do
      let updatedQueue = queue + middleChild + leftChild + rightChild
      process callback updatedQueue solutions exclusions

data Action exclusions solution where
  Return :: NonEmpty solution -> Action NoExclusions solution
  Recurse :: Action exclusions solution
  Pass :: Action exclusions solution

return :: NonEmpty solution -> Action NoExclusions solution
return = Return

recurse :: Action exclusions solution
recurse = Recurse

pass :: Action exclusions solution
pass = Pass

monotonic ::
  Tolerance units =>
  (Float -> Qty units) ->
  (Float -> Qty units) ->
  Range Unitless ->
  Float
monotonic function derivative range = do
  let Range x1 x2 = range
  let y1 = function x1
  let y2 = function x2
  if
    | y1 == Qty.zero -> x1
    | y2 == Qty.zero -> x2
    | Qty.sign y1 == Qty.sign y2 -> if Qty.abs y1 <= Qty.abs y2 then x1 else x2
    | otherwise -> solveMonotonic function derivative range (Qty.sign y1) x1 x2

solveMonotonic ::
  Tolerance units =>
  (Float -> Qty units) ->
  (Float -> Qty units) ->
  Range Unitless ->
  Sign ->
  Float ->
  Float ->
  Float
solveMonotonic function derivative range sign1 x1 x2 = do
  -- First, try applying Newton-Raphson within [x1,x2]
  -- to see if that converges to a root
  let xMid = Qty.midpoint x1 x2
  let yMid = function xMid
  case newtonRaphson function derivative range xMid yMid 0 of
    Success x -> x -- Newton-Raphson converged to a root, return it
    Failure Divergence -- Newton-Raphson did not converge within [x1, x2]
      | x1 < xMid && xMid < x2 ->
          -- It's possible to bisect further,
          -- so recurse into whichever subdomain brackets the root
          if Qty.sign yMid == sign1
            then solveMonotonic function derivative range sign1 xMid x2
            else solveMonotonic function derivative range sign1 x1 xMid
      | otherwise -> xMid -- We've converged to a root by bisection

data Divergence = Divergence deriving (Eq, Show, Error.Message)

newtonRaphson ::
  Tolerance units =>
  (Float -> Qty units) ->
  (Float -> Qty units) ->
  Range Unitless ->
  Float ->
  Qty units ->
  Int ->
  Result Divergence Float
newtonRaphson function derivative range x y iterations =
  if iterations > 10 -- Check if we've entered an infinite loop
    then Failure Divergence
    else do
      let dy = derivative x
      if dy == Qty.zero -- Can't take Newton step if derivative is zero
        then Failure Divergence
        else do
          let x2 = Range.clampTo range (x - y / dy) -- Apply (bounded) Newton step
          let y2 = function x2
          if Qty.abs y2 >= Qty.abs y
            then -- We've stopped converging, check if we're actually at a root
              if y ~= Qty.zero then Success x else Failure Divergence
            else -- We're still converging, so take another iteration
              newtonRaphson function derivative range x2 y2 (iterations + 1)
