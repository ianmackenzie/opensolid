module OpenSolid.Estimate
  ( Estimate
  , Interface (..)
  , new
  , exact
  , bounds
  , refine
  , satisfy
  , within
  , resolve
  , abs
  , sum
  , min
  , max
  , minimumBy
  , maximumBy
  , pickMinimumBy
  , pickMaximumBy
  , sign
  )
where

import Control.Exception qualified
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude hiding (max, min)
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units

class Interface a units | a -> units where
  boundsImpl :: a -> Interval units
  refineImpl :: a -> Estimate units

instance Interface (Quantity units) units where
  boundsImpl = Interval.constant
  refineImpl = exact

data Estimate units where
  Estimate :: Interface a units => a -> Interval units -> Estimate units
  Coerce :: Estimate units1 -> Estimate units2

instance HasUnits (Estimate units) units

instance Units.Coercion (Estimate unitsA) (Estimate unitsB) where
  coerce (Coerce estimate) = Coerce estimate
  coerce estimate = Coerce estimate

new :: Interface a units => a -> Estimate units
new implementation = Estimate implementation (boundsImpl implementation)

exact :: Quantity units -> Estimate units
exact value = new value

bounds :: Estimate units -> Interval units
bounds (Estimate _ cachedBounds) = cachedBounds
bounds (Coerce estimate) = Interval.coerce (bounds estimate)

refine :: Estimate units -> Estimate units
refine estimate = checkRefinement 0 estimate

data RefinementStalled = RefinementStalled deriving (Show)

instance Exception RefinementStalled where
  displayException RefinementStalled = "Estimate refinement stalled"

checkRefinement :: Int -> Estimate units -> Estimate units
checkRefinement stepsWithoutProgress estimate = case estimate of
  Coerce inner -> Coerce (checkRefinement stepsWithoutProgress inner)
  Estimate implementation initialBounds -> do
    let refinedEstimate = refineImpl implementation
    if
      | Interval.width (bounds refinedEstimate) < initialBounds.width -> refinedEstimate
      | stepsWithoutProgress < 10 -> checkRefinement (stepsWithoutProgress + 1) refinedEstimate
      | otherwise -> throw RefinementStalled

satisfy :: (Interval units -> Bool) -> Estimate units -> Interval units
satisfy predicate estimate = do
  let current = bounds estimate
  if predicate current then current else satisfy predicate (refine estimate)

within :: Quantity units -> Estimate units -> Interval units
within tolerance = satisfy (\current -> current.width <= tolerance)

resolve :: (Interval units -> Fuzzy a) -> Estimate units -> a
resolve function estimate =
  case function (bounds estimate) of
    Resolved value -> value
    Unresolved -> resolve function (refine estimate)

newtype Negate units = Negate (Estimate units)

instance Interface (Negate units) units where
  boundsImpl (Negate estimate) = negative (bounds estimate)
  refineImpl (Negate estimate) = negative (refine estimate)

instance Negation (Estimate units) where
  negative estimate = new (Negate estimate)

instance Multiplication Sign (Estimate units) (Estimate units) where
  Positive .*. estimate = estimate
  Negative .*. estimate = negative estimate

instance Multiplication (Estimate units) Sign (Estimate units) where
  estimate .*. Positive = estimate
  estimate .*. Negative = negative estimate

data Add units = Add (Estimate units) (Estimate units)

instance Interface (Add units) units where
  boundsImpl (Add first second) = bounds first .+. bounds second
  refineImpl (Add first second) = do
    let width1 = Interval.width (bounds first)
    let width2 = Interval.width (bounds second)
    if
      | width1 >= 2 *. width2 -> refine first .+. second
      | width2 >= 2 *. width1 -> first .+. refine second
      | otherwise -> refine first .+. refine second

instance Addition (Estimate units) (Estimate units) (Estimate units) where
  first .+. second = new (Add first second)

instance Addition (Estimate units) (Quantity units) (Estimate units) where
  estimate .+. value = estimate .+. exact value

instance Addition (Quantity units) (Estimate units) (Estimate units) where
  value .+. estimate = exact value .+. estimate

data Subtract units = Subtract (Estimate units) (Estimate units)

instance Interface (Subtract units) units where
  boundsImpl (Subtract first second) = bounds first .-. bounds second
  refineImpl (Subtract first second) = do
    let width1 = Interval.width (bounds first)
    let width2 = Interval.width (bounds second)
    if
      | width1 >= 2 *. width2 -> refine first .-. second
      | width2 >= 2 *. width1 -> first .-. refine second
      | otherwise -> refine first .-. refine second

instance Subtraction (Estimate units) (Estimate units) (Estimate units) where
  first .-. second = new (Subtract first second)

instance Subtraction (Estimate units) (Quantity units) (Estimate units) where
  estimate .-. value = estimate .-. exact value

instance Subtraction (Quantity units) (Estimate units) (Estimate units) where
  value .-. estimate = exact value .-. estimate

data Product units1 units2 = Product (Estimate units1) (Estimate units2)

instance Interface (Product units1 units2) (units1 ?*? units2) where
  boundsImpl (Product first second) = bounds first ?*? bounds second
  refineImpl (Product first second) = do
    let firstBounds = bounds first
    let secondBounds = bounds second
    let firstWidth = Interval.width firstBounds
    let secondWidth = Interval.width secondBounds
    let firstMetric = firstWidth ?*? Interval.midpoint secondBounds
    let secondMetric = Interval.midpoint firstBounds ?*? secondWidth
    let combinedMetric = firstWidth ?*? secondWidth
    let refinedProduct
          | firstMetric > secondMetric && firstMetric > combinedMetric =
              Product (refine first) second
          | secondMetric > firstMetric && secondMetric > combinedMetric =
              Product first (refine second)
          | otherwise =
              Product (refine first) (refine second)
    new refinedProduct

instance Multiplication_ (Estimate units1) (Estimate units2) (Estimate (units1 ?*? units2)) where
  first ?*? second = new (Product first second)

instance Multiplication_ (Estimate units1) (Quantity units2) (Estimate (units1 ?*? units2)) where
  estimate ?*? value = new (Product estimate (exact value))

instance Multiplication_ (Quantity units1) (Estimate units2) (Estimate (units1 ?*? units2)) where
  value ?*? estimate = new (Product (exact value) estimate)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Estimate units1)
    (Estimate units2)
    (Estimate units3)
  where
  first .*. second = Units.specialize (first ?*? second)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Estimate units1)
    (Quantity units2)
    (Estimate units3)
  where
  estimate .*. value = estimate .*. exact value

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Quantity units1)
    (Estimate units2)
    (Estimate units3)
  where
  value .*. estimate = exact value .*. estimate

instance Division_ (Estimate units1) (Quantity units2) (Estimate (units1 ?/? units2)) where
  estimate ?/? value = Units.simplify (estimate ?*? (1 /? value))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Estimate units1)
    (Quantity units2)
    (Estimate units3)
  where
  estimate ./. value = Units.specialize (estimate ?/? value)

newtype Sum units = Sum (NonEmpty (Estimate units))

instance Interface (Sum units) units where
  boundsImpl (Sum estimates) = NonEmpty.sum (NonEmpty.map bounds estimates)
  refineImpl (Sum estimates) = do
    let maxWidth = NonEmpty.maximumOf boundsWidth estimates
    let refinedEstimates = NonEmpty.map (refineWiderThan (0.5 *. maxWidth)) estimates
    new (Sum refinedEstimates)

newtype Abs units = Abs (Estimate units)

instance Interface (Abs units) units where
  boundsImpl (Abs estimate) = Interval.abs (bounds estimate)
  refineImpl (Abs estimate) = new (Abs (refine estimate))

abs :: Estimate units -> Estimate units
abs estimate = new (Abs estimate)

refineWiderThan :: Quantity units -> Estimate units -> Estimate units
refineWiderThan maxWidth estimate
  | Quantity.isInfinite (boundsWidth estimate) = refine estimate
  | boundsWidth estimate > maxWidth = refine estimate
  | otherwise = estimate

sum :: NonEmpty (Estimate units) -> Estimate units
sum = new . Sum

data Min units = Min (Estimate units) (Estimate units)

instance Interface (Min units) units where
  boundsImpl (Min first second) = Interval.min (bounds first) (bounds second)
  refineImpl (Min first second) = do
    let Interval min1 max1 = bounds first
    let Interval min2 max2 = bounds second
    if
      | max1 <= min2 -> refine first
      | max2 <= min1 -> refine second
      | otherwise -> min (refine first) (refine second)

min :: Estimate units -> Estimate units -> Estimate units
min first second = new (Min first second)

data Max units = Max (Estimate units) (Estimate units)

instance Interface (Max units) units where
  boundsImpl (Max first second) = Interval.max (bounds first) (bounds second)
  refineImpl (Max first second) = do
    let Interval min1 max1 = bounds first
    let Interval min2 max2 = bounds second
    if
      | min1 >= max2 -> refine first
      | min2 >= max1 -> refine second
      | otherwise -> max (refine first) (refine second)

max :: Estimate units -> Estimate units -> Estimate units
max first second = new (Max first second)

boundsWidth :: Estimate units -> Quantity units
boundsWidth estimate = Interval.width (bounds estimate)

estimateUpperBound :: (a, Estimate units) -> Quantity units
estimateUpperBound (_, estimate) = Interval.upper (bounds estimate)

estimateUpperBoundAtLeast :: Quantity units -> (a, Estimate units) -> Bool
estimateUpperBoundAtLeast cutoff pair = estimateUpperBound pair >= cutoff

estimateLowerBound :: (a, Estimate units) -> Quantity units
estimateLowerBound (_, estimate) = Interval.lower (bounds estimate)

estimateLowerBoundAtMost :: Quantity units -> (a, Estimate units) -> Bool
estimateLowerBoundAtMost cutoff pair = estimateLowerBound pair <= cutoff

itemBoundsWidth :: (a, Estimate units) -> Quantity units
itemBoundsWidth (_, estimate) = Interval.width (bounds estimate)

refinePairs :: NonEmpty (a, Estimate units) -> NonEmpty (a, Estimate units)
refinePairs pairs = do
  let widthCutoff = 0.5 *. NonEmpty.maximumOf itemBoundsWidth pairs
  NonEmpty.map (Pair.mapSecond (refineWiderThan widthCutoff)) pairs

prependItems :: List (a, Estimate units) -> List a -> List a
prependItems pairs items =
  List.foldr (\(item, _) acc -> item : acc) items pairs

isResolved :: Tolerance units => Estimate units -> Bool
isResolved estimate = boundsWidth estimate ~= Quantity.zero

allResolved :: Tolerance units => List (a, Estimate units) -> Bool
allResolved pairs = List.allSatisfy (isResolved . Pair.second) pairs

minimumBy :: Tolerance units => (a -> Estimate units) -> NonEmpty a -> a
minimumBy function items = go (NonEmpty.map (Pair.decorate function) items)
 where
  go pairs = do
    let (leader, followers) = NonEmpty.pickMinimumBy estimateUpperBound pairs
    let cutoff = estimateUpperBound leader
    let filtered = List.filter (estimateLowerBoundAtMost cutoff) followers
    if allResolved filtered
      then Pair.first leader
      else go (refinePairs (leader :| filtered))

maximumBy :: Tolerance units => (a -> Estimate units) -> NonEmpty a -> a
maximumBy function items = go (NonEmpty.map (Pair.decorate function) items)
 where
  go pairs = do
    let (leader, followers) = NonEmpty.pickMaximumBy estimateLowerBound pairs
    let cutoff = estimateLowerBound leader
    let filtered = List.filter (estimateUpperBoundAtLeast cutoff) followers
    if allResolved filtered
      then Pair.first leader
      else go (refinePairs (leader :| filtered))

pickMinimumBy :: Tolerance units => (a -> Estimate units) -> NonEmpty a -> (a, List a)
pickMinimumBy function items = go (NonEmpty.map (Pair.decorate function) items) []
 where
  go pairs accumulated = do
    let (leader, followers) = NonEmpty.pickMinimumBy estimateUpperBound pairs
    let cutoff = estimateUpperBound leader
    let (filtered, discarded) = List.partition (estimateLowerBoundAtMost cutoff) followers
    let updated = prependItems discarded accumulated
    if allResolved filtered
      then (Pair.first leader, prependItems filtered updated)
      else go (refinePairs (leader :| filtered)) updated

pickMaximumBy :: Tolerance units => (a -> Estimate units) -> NonEmpty a -> (a, List a)
pickMaximumBy function items = go (NonEmpty.map (Pair.decorate function) items) []
 where
  go pairs accumulated = do
    let (leader, followers) = NonEmpty.pickMaximumBy estimateLowerBound pairs
    let cutoff = estimateLowerBound leader
    let (filtered, discarded) = List.partition (estimateUpperBoundAtLeast cutoff) followers
    let updated = prependItems discarded accumulated
    if allResolved filtered
      then (Pair.first leader, prependItems filtered updated)
      else go (refinePairs (leader :| filtered)) updated

sign :: Tolerance units => Estimate units -> Sign
sign estimate
  | Interval.lower (bounds estimate) > ?tolerance = Positive
  | Interval.upper (bounds estimate) < negative ?tolerance = Negative
  | Interval.width (bounds estimate) ~= Quantity.zero = Positive
  | otherwise = sign (refine estimate)
