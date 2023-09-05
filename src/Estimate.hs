module Estimate
  ( Estimate
  , IsEstimate (..)
  , wrap
  , exact
  , bounds
  , refine
  , satisfy
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

import Generic qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Pair qualified
import Qty qualified
import Range (Range (Range))
import Range qualified
import Units qualified

class IsEstimate a units | a -> units where
  boundsImpl :: a -> Range units
  refineImpl :: a -> Estimate units

instance IsEstimate (Qty units) units where
  boundsImpl = Range.constant
  refineImpl = exact

data Estimate units where
  Estimate :: IsEstimate a units => a -> Range units -> Estimate units

instance
  ( units1 ~ units1'
  , units2 ~ units2'
  ) =>
  Units.Coercion units1 units2 (Estimate units1') (Estimate units2')

instance units ~ units' => ApproximateEquality (Estimate units) (Qty units') units where
  estimate ~= value
    | Range.minValue (bounds estimate) > value + ?tolerance = False
    | Range.maxValue (bounds estimate) < value - ?tolerance = False
    | Range.width (bounds estimate) <= ?tolerance = True
    | otherwise = refine estimate ~= value

wrap :: IsEstimate a units => a -> Estimate units
wrap implementation = Estimate implementation (boundsImpl implementation)

exact :: Qty units -> Estimate units
exact value = wrap value

bounds :: Estimate units -> Range units
bounds (Estimate _ range) = range

refine :: Estimate units -> Estimate units
refine (Estimate estimate _) = refineImpl estimate

satisfy :: (Range units -> Bool) -> Estimate units -> Range units
satisfy predicate estimate =
  let current = bounds estimate
   in if predicate current then current else satisfy predicate (refine estimate)

instance Generic.HasZero (Estimate units) where
  zeroImpl = exact Qty.zero

newtype Negate units = Negate (Estimate units)

instance IsEstimate (Negate units) units where
  boundsImpl (Negate estimate) = negate (bounds estimate)
  refineImpl (Negate estimate) = negate (refine estimate)

instance Negation (Estimate units) where
  negate estimate = wrap (Negate estimate)

instance Multiplication Sign (Estimate units) (Estimate units) where
  Positive * estimate = estimate
  Negative * estimate = -estimate

instance Multiplication (Estimate units) Sign (Estimate units) where
  estimate * Positive = estimate
  estimate * Negative = -estimate

data Add units = Add (Estimate units) (Estimate units)

instance IsEstimate (Add units) units where
  boundsImpl (Add first second) = bounds first + bounds second
  refineImpl (Add first second)
    | width1 >= 2.0 * width2 = refine first + second
    | width2 >= 2.0 * width1 = first + refine second
    | otherwise = refine first + refine second
   where
    width1 = Range.width (bounds first)
    width2 = Range.width (bounds second)

instance Addition (Estimate units) (Estimate units) (Estimate units) where
  first + second = wrap (Add first second)

data Subtract units = Subtract (Estimate units) (Estimate units)

instance IsEstimate (Subtract units) units where
  boundsImpl (Subtract first second) = bounds first - bounds second
  refineImpl (Subtract first second)
    | width1 >= 2.0 * width2 = refine first - second
    | width2 >= 2.0 * width1 = first - refine second
    | otherwise = refine first - refine second
   where
    width1 = Range.width (bounds first)
    width2 = Range.width (bounds second)

instance Subtraction (Estimate units) (Estimate units) (Estimate units) where
  first - second = wrap (Subtract first second)

newtype Sum units = Sum (NonEmpty (Estimate units))

instance IsEstimate (Sum units) units where
  boundsImpl (Sum estimates) = NonEmpty.sum (NonEmpty.map bounds estimates)
  refineImpl (Sum estimates) =
    let maxWidth = NonEmpty.maximumOf (bounds >> Range.width) estimates
        refinedEstimates = NonEmpty.map (refineWiderThan (0.5 * maxWidth)) estimates
     in wrap (Sum refinedEstimates)

newtype Abs units = Abs (Estimate units)

instance IsEstimate (Abs units) units where
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

instance IsEstimate (Min units) units where
  boundsImpl (Min first second) = Range.min (bounds first) (bounds second)
  refineImpl (Min first second)
    | max1 <= min2 = refine first
    | max2 <= min1 = refine second
    | otherwise = min (refine first) (refine second)
   where
    (Range min1 max1) = bounds first
    (Range min2 max2) = bounds second

min :: Estimate units -> Estimate units -> Estimate units
min first second = wrap (Min first second)

data Max units = Max (Estimate units) (Estimate units)

instance IsEstimate (Max units) units where
  boundsImpl (Max first second) = Range.max (bounds first) (bounds second)
  refineImpl (Max first second)
    | min1 >= max2 = refine first
    | min2 >= max1 = refine second
    | otherwise = max (refine first) (refine second)
   where
    (Range min1 max1) = bounds first
    (Range min2 max2) = bounds second

max :: Estimate units -> Estimate units -> Estimate units
max first second = wrap (Max first second)

data Smaller units = Smaller (Estimate units) (Estimate units)

instance IsEstimate (Smaller units) units where
  boundsImpl (Smaller first second) = Range.smaller (bounds first) (bounds second)
  refineImpl (Smaller first second)
    | high1 <= low2 = refine first
    | high2 <= low1 = refine second
    | otherwise = smaller (refine first) (refine second)
   where
    (Range low1 high1) = Range.abs (bounds first)
    (Range low2 high2) = Range.abs (bounds second)

smaller :: Estimate units -> Estimate units -> Estimate units
smaller first second = wrap (Smaller first second)

data Larger units = Larger (Estimate units) (Estimate units)

instance IsEstimate (Larger units) units where
  boundsImpl (Larger first second) = Range.larger (bounds first) (bounds second)
  refineImpl (Larger first second)
    | low1 >= high2 = refine first
    | low2 >= high1 = refine second
    | otherwise = larger (refine first) (refine second)
   where
    (Range low1 high1) = Range.abs (bounds first)
    (Range low2 high2) = Range.abs (bounds second)

larger :: Estimate units -> Estimate units -> Estimate units
larger first second = wrap (Larger first second)

internalErrorFilteredListIsEmpty :: a
internalErrorFilteredListIsEmpty =
  internalError "Filtered list should be non-empty by construction"

data Smallest units = Smallest (NonEmpty (Estimate units)) (Range units)

instance IsEstimate (Smallest units) units where
  boundsImpl (Smallest _ currentBounds) = currentBounds
  refineImpl (Smallest estimates currentBounds) =
    case NonEmpty.filter (bounds >> Range.intersects currentBounds) estimates of
      [singleEstimate] -> refine singleEstimate
      NonEmpty filteredEstimates ->
        let maxWidth = NonEmpty.maximumOf (bounds >> Range.width) filteredEstimates
            refinedEstimates = NonEmpty.map (refineWiderThan (0.5 * maxWidth)) filteredEstimates
         in smallest refinedEstimates
      [] -> internalErrorFilteredListIsEmpty

smallest :: NonEmpty (Estimate units) -> Estimate units
smallest estimates =
  wrap (Smallest estimates (Range.smallest (NonEmpty.map bounds estimates)))

data Largest units = Largest (NonEmpty (Estimate units)) (Range units)

instance IsEstimate (Largest units) units where
  boundsImpl (Largest _ currentBounds) = currentBounds
  refineImpl (Largest estimates currentBounds) =
    case NonEmpty.filter (bounds >> Range.intersects currentBounds) estimates of
      [singleEstimate] -> refine singleEstimate
      NonEmpty filteredEstimates ->
        let maxWidth = NonEmpty.maximumOf (bounds >> Range.width) filteredEstimates
            refinedEstimates = NonEmpty.map (refineWiderThan (0.5 * maxWidth)) filteredEstimates
         in largest refinedEstimates
      [] -> internalErrorFilteredListIsEmpty

largest :: NonEmpty (Estimate units) -> Estimate units
largest estimates =
  wrap (Largest estimates (Range.largest (NonEmpty.map bounds estimates)))

itemUpperBound :: (a, Estimate units) -> Qty units
itemUpperBound (_, estimate) = Range.maxValue (bounds estimate)

itemLowerBound :: (a, Estimate units) -> Qty units
itemLowerBound (_, estimate) = Range.minValue (bounds estimate)

itemBoundsWidth :: (a, Estimate units) -> Qty units
itemBoundsWidth (_, estimate) = Range.width (bounds estimate)

refinePairs :: NonEmpty (a, Estimate units) -> NonEmpty (a, Estimate units)
refinePairs pairs =
  let widthCutoff = 0.5 * NonEmpty.maximumOf itemBoundsWidth pairs
   in NonEmpty.map (Pair.mapSecond (refineWiderThan widthCutoff)) pairs

prependItems :: List (a, Estimate units) -> List a -> List a
prependItems pairs items =
  List.foldRight (\(item, _) acc -> item : acc) items pairs

allResolved :: Tolerance units => List (a, Estimate units) -> Bool
allResolved pairs = List.all (itemBoundsWidth >> (<= ?tolerance)) pairs

minimumBy :: Tolerance units => (a -> Estimate units) -> NonEmpty a -> a
minimumBy function items = go (NonEmpty.map (\item -> (item, function item)) items)
 where
  go pairs =
    let (leader, followers) = NonEmpty.pickMinimumBy itemUpperBound pairs
        cutoff = itemUpperBound leader
        filtered = List.filter (itemLowerBound >> (<= cutoff)) followers
     in if allResolved filtered
          then Pair.first leader
          else go (refinePairs (leader :| filtered))

maximumBy :: Tolerance units => (a -> Estimate units) -> NonEmpty a -> a
maximumBy function items = go (NonEmpty.map (\item -> (item, function item)) items)
 where
  go pairs =
    let (leader, followers) = NonEmpty.pickMaximumBy itemLowerBound pairs
        cutoff = itemLowerBound leader
        filtered = List.filter (itemUpperBound >> (>= cutoff)) followers
     in if allResolved filtered
          then Pair.first leader
          else go (refinePairs (leader :| filtered))

smallestBy :: Tolerance units => (a -> Estimate units) -> NonEmpty a -> a
smallestBy function items = minimumBy (function >> abs) items

largestBy :: Tolerance units => (a -> Estimate units) -> NonEmpty a -> a
largestBy function items = maximumBy (function >> abs) items

pickMinimumBy :: Tolerance units => (a -> Estimate units) -> NonEmpty a -> (a, List a)
pickMinimumBy function items = go (NonEmpty.map (\item -> (item, function item)) items) []
 where
  go pairs accumulated =
    let (leader, followers) = NonEmpty.pickMinimumBy itemUpperBound pairs
        cutoff = itemUpperBound leader
        (filtered, discarded) = List.partition (itemLowerBound >> (<= cutoff)) followers
        updated = prependItems discarded accumulated
     in if allResolved filtered
          then (Pair.first leader, prependItems filtered updated)
          else go (refinePairs (leader :| filtered)) updated

pickMaximumBy :: Tolerance units => (a -> Estimate units) -> NonEmpty a -> (a, List a)
pickMaximumBy function items = go (NonEmpty.map (\item -> (item, function item)) items) []
 where
  go pairs accumulated =
    let (leader, followers) = NonEmpty.pickMaximumBy itemLowerBound pairs
        cutoff = itemLowerBound leader
        (filtered, discarded) = List.partition (itemUpperBound >> (>= cutoff)) followers
        updated = prependItems discarded accumulated
     in if allResolved filtered
          then (Pair.first leader, prependItems filtered updated)
          else go (refinePairs (leader :| filtered)) updated

pickSmallestBy :: Tolerance units => (a -> Estimate units) -> NonEmpty a -> (a, List a)
pickSmallestBy function items = pickMinimumBy (function >> abs) items

pickLargestBy :: Tolerance units => (a -> Estimate units) -> NonEmpty a -> (a, List a)
pickLargestBy function items = pickMaximumBy (function >> abs) items

sign :: Tolerance units => Estimate units -> Sign
sign estimate
  | Range.minValue (bounds estimate) > ?tolerance = Positive
  | Range.maxValue (bounds estimate) < negate ?tolerance = Negative
  | Range.width (bounds estimate) <= ?tolerance = Positive
  | otherwise = sign (refine estimate)
