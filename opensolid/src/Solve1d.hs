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
  , InfiniteRecursion (InfiniteRecursion)
  , run
  , Action
  , recurse
  , return
  )
where

import Float qualified
import Int qualified
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

data Cache a
  = Atomic Subdomain a
  | Split Subdomain a ~(Cache a) ~(Cache a) ~(Cache a)
  | Shrink Subdomain a ~(Cache a)

init :: (Range Unitless -> a) -> Cache a
init function = split function domain

split :: (Range Unitless -> a) -> Subdomain -> Cache a
split function subdomain = do
  let cached = function (bounds subdomain)
  if isAtomic subdomain
    then Atomic subdomain cached
    else do
      let middleSubdomain = half subdomain
      let (leftSubdomain, rightSubdomain) = bisect subdomain
      let middleChild = shrink function middleSubdomain
      let leftChild = split function leftSubdomain
      let rightChild = split function rightSubdomain
      Split subdomain cached middleChild leftChild rightChild

shrink :: (Range Unitless -> a) -> Subdomain -> Cache a
shrink function subdomain = do
  let cached = function (bounds subdomain)
  if isAtomic subdomain
    then Atomic subdomain cached
    else do
      let child = shrink function (half subdomain)
      Shrink subdomain cached child

data InfiniteRecursion = InfiniteRecursion deriving (Eq, Show, Error)

run :: b -> (Subdomain -> a -> b -> Action b) -> Cache a -> Result InfiniteRecursion b
run initialState processSubdomain cache =
  process initialState processSubdomain (Queue.singleton cache)

process :: b -> (Subdomain -> a -> b -> Action b) -> Queue (Cache a) -> Result InfiniteRecursion b
process currentState processSubdomain queue =
  case Queue.pop queue of
    Just (first, remaining) -> case first of
      Atomic subdomain cached ->
        case processSubdomain subdomain cached currentState of
          Return updatedState -> process updatedState processSubdomain remaining
          Recurse _ -> Error InfiniteRecursion
      Split subdomain cached middleChild leftChild rightChild ->
        case processSubdomain subdomain cached currentState of
          Return updatedState -> process updatedState processSubdomain remaining
          Recurse updatedState -> do
            let updatedQueue = remaining + middleChild + leftChild + rightChild
            process updatedState processSubdomain updatedQueue
      Shrink subdomain cached child ->
        case processSubdomain subdomain cached currentState of
          Return updatedState -> process updatedState processSubdomain remaining
          Recurse updatedState -> do
            let updatedQueue = remaining + child
            process updatedState processSubdomain updatedQueue
    Nothing -> Ok currentState

data Action a
  = Return a
  | Recurse a

return :: a -> Action a
return = Return

recurse :: a -> Action a
recurse = Recurse
