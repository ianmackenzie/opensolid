module OpenSolid.Solve1D
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

import OpenSolid.Curve1D.Zero (Zero (Zero))
import OpenSolid.Domain1D (Domain1D)
import OpenSolid.Domain1D qualified as Domain1D
import OpenSolid.Int qualified as Int
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Queue (Queue)
import OpenSolid.Queue qualified as Queue
import OpenSolid.Result qualified as Result

data Neighborhood units = Neighborhood
  { n :: Int
  , sign :: Sign
  , magnitude :: Quantity units
  , radius :: Number
  }

deriving instance Show (Neighborhood units)

neighborhood :: Tolerance units => Int -> Quantity units -> Neighborhood units
neighborhood n value = do
  let sign = Quantity.sign value
  let magnitude = Quantity.abs value
  let radius =
        Number.pow
          (Number.fromInt (Int.factorial n) .*. ?tolerance ./. magnitude)
          (1 /. Number.fromInt n)
  Neighborhood{n, sign, magnitude, radius}

derivativeTolerance :: Neighborhood units -> Int -> Quantity units
derivativeTolerance Neighborhood{n, magnitude, radius} k = do
  (magnitude .*. Number.pow radius (Number.fromInt (n - k)))
    ./. Number.fromInt (Int.factorial (n - k))

zero :: Number -> Neighborhood units -> Zero
zero location Neighborhood{n, sign} = Zero location (n - 1) sign

data Cache cached
  = Tree Domain1D cached (Node cached)

data Node cached
  = Atomic
  | Splittable ~(Cache cached) ~(Cache cached) ~(Cache cached)
  | Shrinkable ~(Cache cached)

init :: (Interval Unitless -> cached) -> Cache cached
init function = split function Domain1D.unit

tree :: (Interval Unitless -> cached) -> Domain1D -> Node cached -> Cache cached
tree function subdomain givenNode = do
  let cached = function (Domain1D.bounds subdomain)
  let node = if Domain1D.isAtomic subdomain then Atomic else givenNode
  Tree subdomain cached node

split :: (Interval Unitless -> cached) -> Domain1D -> Cache cached
split function subdomain = do
  let middleSubdomain = Domain1D.half subdomain
  let (leftSubdomain, rightSubdomain) = Domain1D.bisect subdomain
  let middleChild = shrink function middleSubdomain
  let leftChild = split function leftSubdomain
  let rightChild = split function rightSubdomain
  tree function subdomain (Splittable middleChild leftChild rightChild)

shrink :: (Interval Unitless -> cached) -> Domain1D -> Cache cached
shrink function subdomain = do
  let child = shrink function (Domain1D.half subdomain)
  tree function subdomain (Shrinkable child)

data NoExclusions

data SomeExclusions

data Exclusions exclusions where
  NoExclusions :: Exclusions NoExclusions
  SomeExclusions :: Exclusions SomeExclusions

data InfiniteRecursion = InfiniteRecursion deriving (Eq, Show)

type Callback cached solution =
  forall exclusions.
  Domain1D ->
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
  List Domain1D ->
  Result InfiniteRecursion (List solution, List Domain1D)
process callback queue solutions exclusions =
  case Queue.pop queue of
    Nothing -> Ok (solutions, exclusions) -- We're done! No more subdomains to process
    Just (Tree subdomain cached node, remaining) -> do
      let filteredExclusions = List.filter (Domain1D.overlaps subdomain) exclusions
      if containedBy filteredExclusions subdomain
        then process callback remaining solutions exclusions
        else case filteredExclusions of
          [] -> case callback subdomain cached NoExclusions of
            Pass -> process callback remaining solutions exclusions
            Recurse -> recurseIntoChildrenOf node callback remaining solutions exclusions
            Return newSolutions -> do
              let updatedSolutions = NonEmpty.toList newSolutions <> solutions
              let updatedExclusions = subdomain : exclusions
              process callback remaining updatedSolutions updatedExclusions
          List.OneOrMore -> case callback subdomain cached SomeExclusions of
            Pass -> process callback remaining solutions exclusions
            Recurse -> recurseIntoChildrenOf node callback remaining solutions exclusions

containedBy :: List Domain1D -> Domain1D -> Bool
containedBy exclusions subdomain =
  Number.sum (List.map (Domain1D.intersectionWidth subdomain) exclusions) == Domain1D.width subdomain

{-# INLINE recurseIntoChildrenOf #-}
recurseIntoChildrenOf ::
  Node cached ->
  Callback cached solution ->
  Queue (Cache cached) ->
  List solution ->
  List Domain1D ->
  Result InfiniteRecursion (List solution, List Domain1D)
recurseIntoChildrenOf node callback queue solutions exclusions = do
  let continueWith updatedQueue = process callback updatedQueue solutions exclusions
  case node of
    Atomic -> Error InfiniteRecursion
    Shrinkable child -> continueWith (queue .+. child)
    Splittable middleChild leftChild rightChild ->
      continueWith (queue .+. middleChild .+. leftChild .+. rightChild)

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

data Root = Exact Number | Closest Number

monotonic ::
  Tolerance units =>
  (Number -> Quantity units) ->
  (Number -> Quantity units) ->
  Interval Unitless ->
  Root
monotonic function derivative interval = do
  let Interval x1 x2 = interval
  let y1 = function x1
  let y2 = function x2
  if
    | y1 == Quantity.zero -> Exact x1
    | y2 == Quantity.zero -> Exact x2
    | Quantity.sign y1 == Quantity.sign y2 ->
        if Quantity.abs y1 <= Quantity.abs y2 then Closest x1 else Closest x2
    | otherwise -> solveMonotonic function derivative interval (Quantity.sign y1) x1 x2

solveMonotonic ::
  Tolerance units =>
  (Number -> Quantity units) ->
  (Number -> Quantity units) ->
  Interval Unitless ->
  Sign ->
  Number ->
  Number ->
  Root
solveMonotonic function derivative interval sign1 x1 x2 = do
  -- First, try applying Newton-Raphson within [x1,x2]
  -- to see if that converges to a zero
  let xMid = Quantity.midpoint x1 x2
  let yMid = function xMid
  if yMid == Quantity.zero
    then Exact xMid
    else case newtonRaphson function derivative interval xMid yMid 0 of
      Ok root -> Exact root -- Newton-Raphson converged to a zero, return it
      Error Divergence -- Newton-Raphson did not converge within [x1, x2]
        | x1 < xMid && xMid < x2 ->
            -- It's possible to bisect further,
            -- so recurse into whichever subdomain brackets the zero
            if Quantity.sign yMid == sign1
              then solveMonotonic function derivative interval sign1 xMid x2
              else solveMonotonic function derivative interval sign1 x1 xMid
        | otherwise -> Exact xMid -- We've converged to a zero by bisection

data Divergence = Divergence deriving (Eq, Show)

newtonRaphson ::
  Tolerance units =>
  (Number -> Quantity units) ->
  (Number -> Quantity units) ->
  Interval Unitless ->
  Number ->
  Quantity units ->
  Int ->
  Result Divergence Number
newtonRaphson function derivative interval x y iterations =
  if iterations > 10 -- Check if we've entered an infinite loop
    then Error Divergence
    else do
      let dy = derivative x
      if dy == Quantity.zero -- Can't take Newton step if derivative is zero
        then Error Divergence
        else do
          let xStepped = x .-. y ./. dy
          x2 <-
            if Interval.includes xStepped interval
              then Ok xStepped -- Newton step stayed within interval
              else do
                -- Newton step went outside interval,
                -- attempt to recover by making another step
                -- starting at the boundary
                let xClamped = Quantity.clampTo interval xStepped
                let yClamped = function xClamped
                let dyClamped = derivative xClamped
                if dyClamped == Quantity.zero
                  then Error Divergence
                  else do
                    let xStepped2 = xClamped .-. yClamped ./. dyClamped
                    if Interval.includes xStepped2 interval
                      then Ok xStepped2
                      else Error Divergence
          let y2 = function x2
          if Quantity.abs y2 >= Quantity.abs y
            then -- We've stopped converging, check if we're actually at a root
              if y ~= Quantity.zero then Ok x else Error Divergence
            else -- We're still converging, so take another iteration
              newtonRaphson function derivative interval x2 y2 (iterations + 1)
