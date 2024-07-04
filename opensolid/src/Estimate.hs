module Estimate
  ( Estimate
  , Interface (..)
  , wrap
  , exact
  , bounds
  , refine
  , satisfy
  , resolve
  , abs
  , sum
  , min
  , max
  , smaller
  , larger
  , smallest
  , largest
  , minimumBy
  , maximumBy
  , smallestBy
  , largestBy
  , pickMinimumBy
  , pickMaximumBy
  , pickSmallestBy
  , pickLargestBy
  , sign
  )
where

import Float qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Pair qualified
import Qty qualified
import Range (Range (Range))
import Range qualified
import Units qualified

class Interface a units | a -> units where
  boundsImpl :: a -> Range units
  refineImpl :: a -> Estimate units

instance Interface (Qty units) units where
  boundsImpl = Range.constant
  refineImpl = exact

data Estimate units where
  Estimate :: Interface a units => a -> Range units -> Estimate units
  Coerce :: Estimate units1 -> Estimate units2

instance HasUnits (Estimate units) where
  type Units (Estimate units) = units
  type Erase (Estimate units) = Estimate Unitless

instance Units.Coercion (Estimate units1) (Estimate units2) where
  coerce (Coerce estimate) = Coerce estimate
  coerce estimate = Coerce estimate

instance units ~ units_ => ApproximateEquality (Estimate units) (Qty units_) units where
  estimate ~= value
    | bounds estimate ~= value = True
    | not (value ^ bounds estimate) = False
    | otherwise = refine estimate ~= value

wrap :: Interface a units => a -> Estimate units
wrap implementation = Estimate implementation (boundsImpl implementation)

exact :: Qty units -> Estimate units
exact value = wrap value

bounds :: Estimate units -> Range units
bounds (Estimate _ cachedBounds) = cachedBounds
bounds (Coerce estimate) = Units.coerce (bounds estimate)

refine :: Estimate units -> Estimate units
refine (Estimate implementation _) = refineImpl implementation
refine (Coerce estimate) = Coerce (refine estimate)

satisfy :: (Range units -> Bool) -> Estimate units -> Range units
satisfy predicate estimate = do
  let current = bounds estimate
  if predicate current then current else satisfy predicate (refine estimate)

resolve :: (Range units -> Fuzzy a) -> Estimate units -> a
resolve function estimate =
  case function (bounds estimate) of
    Resolved value -> value
    Unresolved -> resolve function (refine estimate)

newtype Negate units = Negate (Estimate units)

instance Interface (Negate units) units where
  boundsImpl (Negate estimate) = negate (bounds estimate)
  refineImpl (Negate estimate) = negate (refine estimate)

instance Negation (Estimate units) where
  negate estimate = wrap (Negate estimate)

instance Multiplication' Sign (Estimate units) where
  type Sign .*. Estimate units = Estimate (Unitless :*: units)
  Positive .*. estimate = Units.coerce estimate
  Negative .*. estimate = Units.coerce -estimate

instance Multiplication Sign (Estimate units) (Estimate units)

instance Multiplication' (Estimate units) Sign where
  type Estimate units .*. Sign = Estimate (units :*: Unitless)
  estimate .*. Positive = Units.coerce estimate
  estimate .*. Negative = Units.coerce -estimate

instance Multiplication (Estimate units) Sign (Estimate units)

data Add units = Add (Estimate units) (Estimate units)

instance Interface (Add units) units where
  boundsImpl (Add first second) = bounds first + bounds second
  refineImpl (Add first second) = do
    let width1 = Range.width (bounds first)
    let width2 = Range.width (bounds second)
    if
      | width1 >= 2 * width2 -> refine first + second
      | width2 >= 2 * width1 -> first + refine second
      | otherwise -> refine first + refine second

instance Addition (Estimate units) (Estimate units) (Estimate units) where
  first + second = wrap (Add first second)

instance Addition (Estimate units) (Qty units) (Estimate units) where
  estimate + value = estimate + exact value

instance Addition (Qty units) (Estimate units) (Estimate units) where
  value + estimate = exact value + estimate

instance Addition (Estimate Unitless) Int (Estimate Unitless) where
  estimate + n = estimate + Float.int n

instance Addition Int (Estimate Unitless) (Estimate Unitless) where
  n + estimate = Float.int n + estimate

data Subtract units = Subtract (Estimate units) (Estimate units)

instance Interface (Subtract units) units where
  boundsImpl (Subtract first second) = bounds first - bounds second
  refineImpl (Subtract first second) = do
    let width1 = Range.width (bounds first)
    let width2 = Range.width (bounds second)
    if
      | width1 >= 2 * width2 -> refine first - second
      | width2 >= 2 * width1 -> first - refine second
      | otherwise -> refine first - refine second

instance Subtraction (Estimate units) (Estimate units) (Estimate units) where
  first - second = wrap (Subtract first second)

instance Subtraction (Estimate units) (Qty units) (Estimate units) where
  estimate - value = estimate - exact value

instance Subtraction (Qty units) (Estimate units) (Estimate units) where
  value - estimate = exact value - estimate

instance Subtraction (Estimate Unitless) Int (Estimate Unitless) where
  estimate - n = estimate - Float.int n

instance Subtraction Int (Estimate Unitless) (Estimate Unitless) where
  n - estimate = Float.int n - estimate

data Product units1 units2 = Product (Estimate units1) (Estimate units2)

instance Interface (Product units1 units2) (units1 :*: units2) where
  boundsImpl (Product first second) = bounds first .*. bounds second
  refineImpl (Product first second) = do
    let firstBounds = bounds first
    let secondBounds = bounds second
    let firstWidth = Range.width firstBounds
    let secondWidth = Range.width secondBounds
    let firstMetric = firstWidth .*. Range.midpoint secondBounds
    let secondMetric = Range.midpoint firstBounds .*. secondWidth
    let combinedMetric = firstWidth .*. secondWidth
    let refinedProduct
          | firstMetric > secondMetric && firstMetric > combinedMetric = Product (refine first) second
          | secondMetric > firstMetric && secondMetric > combinedMetric = Product first (refine second)
          | otherwise = Product (refine first) (refine second)
    wrap refinedProduct

instance Multiplication' (Estimate units1) (Estimate units2) where
  type Estimate units1 .*. Estimate units2 = Estimate (units1 :*: units2)
  first .*. second = wrap (Product first second)

instance Multiplication' (Estimate units1) (Qty units2) where
  type Estimate units1 .*. Qty units2 = Estimate (units1 :*: units2)
  estimate .*. value = wrap (Product estimate (exact value))

instance Multiplication' (Qty units1) (Estimate units2) where
  type Qty units1 .*. Estimate units2 = Estimate (units1 :*: units2)
  value .*. estimate = wrap (Product (exact value) estimate)

instance Multiplication' (Estimate units) Int where
  type Estimate units .*. Int = Estimate (units :*: Unitless)
  estimate .*. n = estimate .*. Float.int n

instance Multiplication' Int (Estimate units) where
  type Int .*. Estimate units = Estimate (Unitless :*: units)
  n .*. estimate = Float.int n .*. estimate

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Estimate units1)
    (Estimate units2)
    (Estimate units3)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Estimate units1)
    (Qty units2)
    (Estimate units3)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Qty units1)
    (Estimate units2)
    (Estimate units3)

instance Multiplication (Estimate units) Int (Estimate units)

instance Multiplication Int (Estimate units) (Estimate units)

instance Division' (Estimate units1) (Qty units2) where
  type Estimate units1 ./. Qty units2 = Estimate (units1 :/: units2)
  estimate ./. value = estimate ^*. (1.0 ./. value)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Estimate units1)
    (Qty units2)
    (Estimate units3)

instance Division' (Estimate units) Int where
  type Estimate units ./. Int = Estimate (units :/: Unitless)
  estimate ./. n = estimate ./. Float.int n

instance Division (Estimate units) Int (Estimate units)

newtype Sum units = Sum (NonEmpty (Estimate units))

instance Interface (Sum units) units where
  boundsImpl (Sum estimates) = NonEmpty.sum (NonEmpty.map bounds estimates)
  refineImpl (Sum estimates) = do
    let maxWidth = NonEmpty.maximumOf boundsWidth estimates
    let refinedEstimates = NonEmpty.map (refineWiderThan (0.5 * maxWidth)) estimates
    wrap (Sum refinedEstimates)

newtype Abs units = Abs (Estimate units)

instance Interface (Abs units) units where
  boundsImpl (Abs estimate) = Range.abs (bounds estimate)
  refineImpl (Abs estimate) = wrap (Abs (refine estimate))

abs :: Estimate units -> Estimate units
abs estimate = wrap (Abs estimate)

refineWiderThan :: Qty units -> Estimate units -> Estimate units
refineWiderThan desiredWidth estimate
  | Range.width (bounds estimate) > desiredWidth = refine estimate
  | otherwise = estimate

sum :: List (Estimate units) -> Estimate units
sum [] = exact Qty.zero
sum (NonEmpty estimates) = wrap (Sum estimates)

data Min units = Min (Estimate units) (Estimate units)

instance Interface (Min units) units where
  boundsImpl (Min first second) = Range.min (bounds first) (bounds second)
  refineImpl (Min first second) = do
    let (Range min1 max1) = bounds first
    let (Range min2 max2) = bounds second
    if
      | max1 <= min2 -> refine first
      | max2 <= min1 -> refine second
      | otherwise -> min (refine first) (refine second)

min :: Estimate units -> Estimate units -> Estimate units
min first second = wrap (Min first second)

data Max units = Max (Estimate units) (Estimate units)

instance Interface (Max units) units where
  boundsImpl (Max first second) = Range.max (bounds first) (bounds second)
  refineImpl (Max first second) = do
    let (Range min1 max1) = bounds first
    let (Range min2 max2) = bounds second
    if
      | min1 >= max2 -> refine first
      | min2 >= max1 -> refine second
      | otherwise -> max (refine first) (refine second)

max :: Estimate units -> Estimate units -> Estimate units
max first second = wrap (Max first second)

data Smaller units = Smaller (Estimate units) (Estimate units)

instance Interface (Smaller units) units where
  boundsImpl (Smaller first second) = Range.smaller (bounds first) (bounds second)
  refineImpl (Smaller first second) = do
    let (Range low1 high1) = Range.abs (bounds first)
    let (Range low2 high2) = Range.abs (bounds second)
    if
      | high1 <= low2 -> refine first
      | high2 <= low1 -> refine second
      | otherwise -> smaller (refine first) (refine second)

smaller :: Estimate units -> Estimate units -> Estimate units
smaller first second = wrap (Smaller first second)

data Larger units = Larger (Estimate units) (Estimate units)

instance Interface (Larger units) units where
  boundsImpl (Larger first second) = Range.larger (bounds first) (bounds second)
  refineImpl (Larger first second) = do
    let (Range low1 high1) = Range.abs (bounds first)
    let (Range low2 high2) = Range.abs (bounds second)
    if
      | low1 >= high2 -> refine first
      | low2 >= high1 -> refine second
      | otherwise -> larger (refine first) (refine second)

larger :: Estimate units -> Estimate units -> Estimate units
larger first second = wrap (Larger first second)

internalErrorFilteredListIsEmpty :: a
internalErrorFilteredListIsEmpty =
  internalError "Filtered list should be non-empty by construction"

boundsWidth :: Estimate units -> Qty units
boundsWidth estimate = Range.width (bounds estimate)

overlaps :: Range units -> Estimate units -> Bool
overlaps range estimate = Range.overlap range (bounds estimate) > Qty.zero

data Smallest units = Smallest (NonEmpty (Estimate units)) (Range units)

instance Interface (Smallest units) units where
  boundsImpl (Smallest _ currentBounds) = currentBounds
  refineImpl (Smallest estimates currentBounds) =
    case NonEmpty.filter (overlaps currentBounds) estimates of
      [singleEstimate] -> refine singleEstimate
      NonEmpty filteredEstimates -> do
        let maxWidth = NonEmpty.maximumOf boundsWidth filteredEstimates
        let refinedEstimates = NonEmpty.map (refineWiderThan (0.5 * maxWidth)) filteredEstimates
        smallest refinedEstimates
      [] -> internalErrorFilteredListIsEmpty

smallest :: NonEmpty (Estimate units) -> Estimate units
smallest estimates =
  wrap (Smallest estimates (Range.smallest (NonEmpty.map bounds estimates)))

data Largest units = Largest (NonEmpty (Estimate units)) (Range units)

instance Interface (Largest units) units where
  boundsImpl (Largest _ currentBounds) = currentBounds
  refineImpl (Largest estimates currentBounds) =
    case NonEmpty.filter (overlaps currentBounds) estimates of
      [singleEstimate] -> refine singleEstimate
      NonEmpty filteredEstimates -> do
        let maxWidth = NonEmpty.maximumOf boundsWidth filteredEstimates
        let refinedEstimates = NonEmpty.map (refineWiderThan (0.5 * maxWidth)) filteredEstimates
        largest refinedEstimates
      [] -> internalErrorFilteredListIsEmpty

largest :: NonEmpty (Estimate units) -> Estimate units
largest estimates =
  wrap (Largest estimates (Range.largest (NonEmpty.map bounds estimates)))

estimateUpperBound :: (a, Estimate units) -> Qty units
estimateUpperBound (_, estimate) = Range.maxValue (bounds estimate)

estimateUpperBoundAtLeast :: Qty units -> (a, Estimate units) -> Bool
estimateUpperBoundAtLeast cutoff pair = estimateUpperBound pair >= cutoff

estimateLowerBound :: (a, Estimate units) -> Qty units
estimateLowerBound (_, estimate) = Range.minValue (bounds estimate)

estimateLowerBoundAtMost :: Qty units -> (a, Estimate units) -> Bool
estimateLowerBoundAtMost cutoff pair = estimateLowerBound pair <= cutoff

itemBoundsWidth :: (a, Estimate units) -> Qty units
itemBoundsWidth (_, estimate) = Range.width (bounds estimate)

refinePairs :: NonEmpty (a, Estimate units) -> NonEmpty (a, Estimate units)
refinePairs pairs = do
  let widthCutoff = 0.5 * NonEmpty.maximumOf itemBoundsWidth pairs
  NonEmpty.map (Pair.mapSecond (refineWiderThan widthCutoff)) pairs

prependItems :: List (a, Estimate units) -> List a -> List a
prependItems pairs items =
  List.foldr (\(item, _) acc -> item : acc) items pairs

isResolved :: Tolerance units => Estimate units -> Bool
isResolved estimate = boundsWidth estimate ~= Qty.zero

allResolved :: Tolerance units => List (a, Estimate units) -> Bool
allResolved pairs = List.allSatisfy (Pair.second >> isResolved) pairs

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

smallestBy :: Tolerance units => (a -> Estimate units) -> NonEmpty a -> a
smallestBy function items = minimumBy (abs . function) items

largestBy :: Tolerance units => (a -> Estimate units) -> NonEmpty a -> a
largestBy function items = maximumBy (abs . function) items

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

pickSmallestBy :: Tolerance units => (a -> Estimate units) -> NonEmpty a -> (a, List a)
pickSmallestBy function items = pickMinimumBy (abs . function) items

pickLargestBy :: Tolerance units => (a -> Estimate units) -> NonEmpty a -> (a, List a)
pickLargestBy function items = pickMaximumBy (abs . function) items

sign :: Tolerance units => Estimate units -> Sign
sign estimate
  | Range.minValue (bounds estimate) > ?tolerance = Positive
  | Range.maxValue (bounds estimate) < negate ?tolerance = Negative
  | Range.width (bounds estimate) ~= Qty.zero = Positive
  | otherwise = sign (refine estimate)
