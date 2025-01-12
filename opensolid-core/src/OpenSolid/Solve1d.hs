{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module OpenSolid.Solve1d
  ( Neighborhood
  , neighborhood
  , derivativeTolerance
  , zero
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
  , Root (Exact, Closest)
  , monotonic
  )
where

import OpenSolid.Curve.Zero (Zero (Zero))
import OpenSolid.Domain1d (Domain1d)
import OpenSolid.Domain1d qualified as Domain1d
import OpenSolid.Error qualified as Error
import OpenSolid.Float qualified as Float
import OpenSolid.Int qualified as Int
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Queue (Queue)
import OpenSolid.Queue qualified as Queue
import OpenSolid.Range (Range (Range))
import OpenSolid.Range qualified as Range
import OpenSolid.Result qualified as Result

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
  let radius = (Float.int (Int.factorial n) * ?tolerance / magnitude) ** (1 / n)
  Neighborhood{n, sign, magnitude, radius}

derivativeTolerance :: Neighborhood units -> Int -> Qty units
derivativeTolerance (Neighborhood{n, magnitude, radius}) k = do
  magnitude * radius ** (n - k) / Float.int (Int.factorial (n - k))

zero :: Float -> Neighborhood units -> Zero
zero location (Neighborhood{n, sign}) = Zero location (n - 1) sign

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
    Nothing -> Success (solutions, exclusions) -- We're done! No more subdomains to process
    Just (Tree subdomain cached node, remaining) -> do
      let filteredExclusions = List.filter (Domain1d.overlaps subdomain) exclusions
      if containedBy filteredExclusions subdomain
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

containedBy :: List Domain1d -> Domain1d -> Bool
containedBy exclusions subdomain =
  Float.sum (List.map (Domain1d.intersectionWidth subdomain) exclusions) == Domain1d.width subdomain

{-# INLINE recurseIntoChildrenOf #-}
recurseIntoChildrenOf ::
  Node cached ->
  Callback cached solution ->
  Queue (Cache cached) ->
  List solution ->
  List Domain1d ->
  Result InfiniteRecursion (List solution, List Domain1d)
recurseIntoChildrenOf node callback queue solutions exclusions = do
  let continueWith updatedQueue = process callback updatedQueue solutions exclusions
  case node of
    Atomic -> Failure InfiniteRecursion
    Shrinkable child -> continueWith (queue + child)
    Splittable middleChild leftChild rightChild ->
      continueWith (queue + middleChild + leftChild + rightChild)

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

data Root = Exact Float | Closest Float

monotonic ::
  Tolerance units =>
  (Float -> Qty units) ->
  (Float -> Qty units) ->
  Range Unitless ->
  Root
monotonic function derivative range = do
  let Range x1 x2 = range
  let y1 = function x1
  let y2 = function x2
  if
    | y1 == Qty.zero -> Exact x1
    | y2 == Qty.zero -> Exact x2
    | Qty.sign y1 == Qty.sign y2 -> if Qty.abs y1 <= Qty.abs y2 then Closest x1 else Closest x2
    | otherwise -> solveMonotonic function derivative range (Qty.sign y1) x1 x2

solveMonotonic ::
  Tolerance units =>
  (Float -> Qty units) ->
  (Float -> Qty units) ->
  Range Unitless ->
  Sign ->
  Float ->
  Float ->
  Root
solveMonotonic function derivative range sign1 x1 x2 = do
  -- First, try applying Newton-Raphson within [x1,x2]
  -- to see if that converges to a zero
  let xMid = Qty.midpoint x1 x2
  let yMid = function xMid
  if yMid == Qty.zero
    then Exact xMid
    else case newtonRaphson function derivative range xMid yMid 0 of
      Success root -> Exact root -- Newton-Raphson converged to a zero, return it
      Failure Divergence -- Newton-Raphson did not converge within [x1, x2]
        | x1 < xMid && xMid < x2 ->
            -- It's possible to bisect further,
            -- so recurse into whichever subdomain brackets the zero
            if Qty.sign yMid == sign1
              then solveMonotonic function derivative range sign1 xMid x2
              else solveMonotonic function derivative range sign1 x1 xMid
        | otherwise -> Exact xMid -- We've converged to a zero by bisection

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
        else Result.do
          let xStepped = x - y / dy
          x2 <-
            if Range.includes xStepped range
              then Success xStepped -- Newton step stayed within range
              else do
                -- Newton step went outside range,
                -- attempt to recover by making another step
                -- starting at the range boundary
                let xClamped = Qty.clampTo range xStepped
                let yClamped = function xClamped
                let dyClamped = derivative xClamped
                if dyClamped == Qty.zero
                  then Failure Divergence
                  else do
                    let xStepped2 = xClamped - yClamped / dyClamped
                    if Range.includes xStepped2 range
                      then Success xStepped2
                      else Failure Divergence
          let y2 = function x2
          if Qty.abs y2 >= Qty.abs y
            then -- We've stopped converging, check if we're actually at a root
              if y ~= Qty.zero then Success x else Failure Divergence
            else -- We're still converging, so take another iteration
              newtonRaphson function derivative range x2 y2 (iterations + 1)
