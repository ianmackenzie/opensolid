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

import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude hiding (max, min, (*), (+), (-), (/))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Units qualified as Units
import Prelude ((+))

class Interface a units | a -> units where
  boundsImpl :: a -> Bounds units
  refineImpl :: a -> Estimate units

instance Interface (Quantity units) units where
  boundsImpl = Bounds.constant
  refineImpl = exact

data Estimate units where
  Estimate :: Interface a units => a -> Bounds units -> Estimate units
  Coerce :: Estimate units1 -> Estimate units2

instance HasField "bounds" (Estimate units) (Bounds units) where
  getField = bounds

instance HasUnits (Estimate units) units

instance Units.Coercion (Estimate unitsA) (Estimate unitsB) where
  coerce (Coerce estimate) = Coerce estimate
  coerce estimate = Coerce estimate

new :: Interface a units => a -> Estimate units
new implementation = Estimate implementation (boundsImpl implementation)

exact :: Quantity units -> Estimate units
exact value = new value

bounds :: Estimate units -> Bounds units
bounds (Estimate _ cachedBounds) = cachedBounds
bounds (Coerce estimate) = Bounds.coerce (bounds estimate)

refine :: Estimate units -> Estimate units
refine estimate = checkRefinement 0 estimate

checkRefinement :: Int -> Estimate units -> Estimate units
checkRefinement stepsWithoutProgress estimate = case estimate of
  Coerce inner -> Coerce (checkRefinement stepsWithoutProgress inner)
  Estimate implementation initialBounds -> do
    let refinedEstimate = refineImpl implementation
    if
      | refinedEstimate.bounds.width < initialBounds.width -> refinedEstimate
      | stepsWithoutProgress < 10 -> checkRefinement (stepsWithoutProgress + 1) refinedEstimate
      | otherwise -> internalError "Estimate refinement stalled"

satisfy :: (Bounds units -> Bool) -> Estimate units -> Bounds units
satisfy predicate estimate = do
  let current = bounds estimate
  if predicate current then current else satisfy predicate (refine estimate)

within :: Quantity units -> Estimate units -> Bounds units
within tolerance = satisfy (\current -> current.width <= tolerance)

resolve :: (Bounds units -> Fuzzy a) -> Estimate units -> a
resolve function estimate =
  case function (bounds estimate) of
    Resolved value -> value
    Unresolved -> resolve function (refine estimate)

newtype Negate units = Negate (Estimate units)

instance Interface (Negate units) units where
  boundsImpl (Negate estimate) = negate (bounds estimate)
  refineImpl (Negate estimate) = negate (refine estimate)

instance Negation (Estimate units) where
  negate estimate = new (Negate estimate)

instance Multiplication Sign (Estimate units) (Estimate units) where
  Positive .*. estimate = estimate
  Negative .*. estimate = -estimate

instance Multiplication (Estimate units) Sign (Estimate units) where
  estimate .*. Positive = estimate
  estimate .*. Negative = -estimate

data Add units = Add (Estimate units) (Estimate units)

instance Interface (Add units) units where
  boundsImpl (Add first second) = bounds first .+. bounds second
  refineImpl (Add first second) = do
    let width1 = Bounds.width (bounds first)
    let width2 = Bounds.width (bounds second)
    if
      | width1 >= 2.0 *. width2 -> refine first .+. second
      | width2 >= 2.0 *. width1 -> first .+. refine second
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
    let width1 = Bounds.width (bounds first)
    let width2 = Bounds.width (bounds second)
    if
      | width1 >= 2.0 *. width2 -> refine first .-. second
      | width2 >= 2.0 *. width1 -> first .-. refine second
      | otherwise -> refine first .-. refine second

instance Subtraction (Estimate units) (Estimate units) (Estimate units) where
  first .-. second = new (Subtract first second)

instance Subtraction (Estimate units) (Quantity units) (Estimate units) where
  estimate .-. value = estimate .-. exact value

instance Subtraction (Quantity units) (Estimate units) (Estimate units) where
  value .-. estimate = exact value .-. estimate

data Product units1 units2 = Product (Estimate units1) (Estimate units2)

instance Interface (Product units1 units2) (units1 #*# units2) where
  boundsImpl (Product first second) = bounds first #*# bounds second
  refineImpl (Product first second) = do
    let firstBounds = bounds first
    let secondBounds = bounds second
    let firstWidth = Bounds.width firstBounds
    let secondWidth = Bounds.width secondBounds
    let firstMetric = firstWidth #*# Bounds.midpoint secondBounds
    let secondMetric = Bounds.midpoint firstBounds #*# secondWidth
    let combinedMetric = firstWidth #*# secondWidth
    let refinedProduct
          | firstMetric > secondMetric && firstMetric > combinedMetric =
              Product (refine first) second
          | secondMetric > firstMetric && secondMetric > combinedMetric =
              Product first (refine second)
          | otherwise =
              Product (refine first) (refine second)
    new refinedProduct

instance Multiplication# (Estimate units1) (Estimate units2) (Estimate (units1 #*# units2)) where
  first #*# second = new (Product first second)

instance Multiplication# (Estimate units1) (Quantity units2) (Estimate (units1 #*# units2)) where
  estimate #*# value = new (Product estimate (exact value))

instance Multiplication# (Quantity units1) (Estimate units2) (Estimate (units1 #*# units2)) where
  value #*# estimate = new (Product (exact value) estimate)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Estimate units1)
    (Estimate units2)
    (Estimate units3)
  where
  first .*. second = Units.specialize (first #*# second)

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

instance Division# (Estimate units1) (Quantity units2) (Estimate (units1 #/# units2)) where
  estimate #/# value = Units.simplify (estimate #*# (1.0 /# value))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Estimate units1)
    (Quantity units2)
    (Estimate units3)
  where
  estimate ./. value = Units.specialize (estimate #/# value)

newtype Sum units = Sum (NonEmpty (Estimate units))

instance Interface (Sum units) units where
  boundsImpl (Sum estimates) = NonEmpty.sum (NonEmpty.map bounds estimates)
  refineImpl (Sum estimates) = do
    let maxWidth = NonEmpty.maximumOf boundsWidth estimates
    let refinedEstimates = NonEmpty.map (refineWiderThan (0.5 *. maxWidth)) estimates
    new (Sum refinedEstimates)

newtype Abs units = Abs (Estimate units)

instance Interface (Abs units) units where
  boundsImpl (Abs estimate) = Bounds.abs (bounds estimate)
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
  boundsImpl (Min first second) = Bounds.min (bounds first) (bounds second)
  refineImpl (Min first second) = do
    let (Bounds min1 max1) = bounds first
    let (Bounds min2 max2) = bounds second
    if
      | max1 <= min2 -> refine first
      | max2 <= min1 -> refine second
      | otherwise -> min (refine first) (refine second)

min :: Estimate units -> Estimate units -> Estimate units
min first second = new (Min first second)

data Max units = Max (Estimate units) (Estimate units)

instance Interface (Max units) units where
  boundsImpl (Max first second) = Bounds.max (bounds first) (bounds second)
  refineImpl (Max first second) = do
    let (Bounds min1 max1) = bounds first
    let (Bounds min2 max2) = bounds second
    if
      | min1 >= max2 -> refine first
      | min2 >= max1 -> refine second
      | otherwise -> max (refine first) (refine second)

max :: Estimate units -> Estimate units -> Estimate units
max first second = new (Max first second)

data Smaller units = Smaller (Estimate units) (Estimate units)

instance Interface (Smaller units) units where
  boundsImpl (Smaller first second) = Bounds.smaller (bounds first) (bounds second)
  refineImpl (Smaller first second) = do
    let (Bounds low1 high1) = Bounds.abs (bounds first)
    let (Bounds low2 high2) = Bounds.abs (bounds second)
    if
      | high1 <= low2 -> refine first
      | high2 <= low1 -> refine second
      | otherwise -> smaller (refine first) (refine second)

smaller :: Estimate units -> Estimate units -> Estimate units
smaller first second = new (Smaller first second)

data Larger units = Larger (Estimate units) (Estimate units)

instance Interface (Larger units) units where
  boundsImpl (Larger first second) = Bounds.larger (bounds first) (bounds second)
  refineImpl (Larger first second) = do
    let (Bounds low1 high1) = Bounds.abs (bounds first)
    let (Bounds low2 high2) = Bounds.abs (bounds second)
    if
      | low1 >= high2 -> refine first
      | low2 >= high1 -> refine second
      | otherwise -> larger (refine first) (refine second)

larger :: Estimate units -> Estimate units -> Estimate units
larger first second = new (Larger first second)

internalErrorFilteredListIsEmpty :: a
internalErrorFilteredListIsEmpty =
  internalError "Filtered list should be non-empty by construction"

boundsWidth :: Estimate units -> Quantity units
boundsWidth estimate = Bounds.width (bounds estimate)

overlaps :: Bounds units -> Estimate units -> Bool
overlaps givenBounds estimate = Bounds.overlap givenBounds (bounds estimate) > Quantity.zero

data Smallest units = Smallest (NonEmpty (Estimate units)) (Bounds units)

instance Interface (Smallest units) units where
  boundsImpl (Smallest _ currentBounds) = currentBounds
  refineImpl (Smallest estimates currentBounds) =
    case NonEmpty.filter (overlaps currentBounds) estimates of
      [singleEstimate] -> refine singleEstimate
      NonEmpty filteredEstimates -> do
        let maxWidth = NonEmpty.maximumOf boundsWidth filteredEstimates
        let refinedEstimates = NonEmpty.map (refineWiderThan (0.5 *. maxWidth)) filteredEstimates
        smallest refinedEstimates
      [] -> internalErrorFilteredListIsEmpty

smallest :: NonEmpty (Estimate units) -> Estimate units
smallest estimates =
  new (Smallest estimates (Bounds.smallest (NonEmpty.map bounds estimates)))

data Largest units = Largest (NonEmpty (Estimate units)) (Bounds units)

instance Interface (Largest units) units where
  boundsImpl (Largest _ currentBounds) = currentBounds
  refineImpl (Largest estimates currentBounds) =
    case NonEmpty.filter (overlaps currentBounds) estimates of
      [singleEstimate] -> refine singleEstimate
      NonEmpty filteredEstimates -> do
        let maxWidth = NonEmpty.maximumOf boundsWidth filteredEstimates
        let refinedEstimates = NonEmpty.map (refineWiderThan (0.5 *. maxWidth)) filteredEstimates
        largest refinedEstimates
      [] -> internalErrorFilteredListIsEmpty

largest :: NonEmpty (Estimate units) -> Estimate units
largest estimates =
  new (Largest estimates (Bounds.largest (NonEmpty.map bounds estimates)))

estimateUpperBound :: (a, Estimate units) -> Quantity units
estimateUpperBound (_, estimate) = Bounds.upper (bounds estimate)

estimateUpperBoundAtLeast :: Quantity units -> (a, Estimate units) -> Bool
estimateUpperBoundAtLeast cutoff pair = estimateUpperBound pair >= cutoff

estimateLowerBound :: (a, Estimate units) -> Quantity units
estimateLowerBound (_, estimate) = Bounds.lower (bounds estimate)

estimateLowerBoundAtMost :: Quantity units -> (a, Estimate units) -> Bool
estimateLowerBoundAtMost cutoff pair = estimateLowerBound pair <= cutoff

itemBoundsWidth :: (a, Estimate units) -> Quantity units
itemBoundsWidth (_, estimate) = Bounds.width (bounds estimate)

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
  | Bounds.lower (bounds estimate) > ?tolerance = Positive
  | Bounds.upper (bounds estimate) < negate ?tolerance = Negative
  | Bounds.width (bounds estimate) ~= Quantity.zero = Positive
  | otherwise = sign (refine estimate)
