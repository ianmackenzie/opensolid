module Estimate
  ( Estimate
  , IsEstimate (..)
  , wrap
  , exact
  , bounds
  , refine
  , satisfy
  , sum
  , min
  , max
  , smaller
  , larger
  )
where

import NonEmpty qualified
import OpenSolid
import Qty qualified
import Range (Range (Range))
import Range qualified

class IsEstimate a units | a -> units where
  boundsImpl :: a -> Range units
  refineImpl :: a -> Estimate units

instance IsEstimate (Qty units) units where
  boundsImpl = Range.constant
  refineImpl = exact

data Estimate units where
  Estimate :: IsEstimate a units => a -> Range units -> Estimate units

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
    let estimateWidths = NonEmpty.map (bounds >> Range.width) estimates
        maxWidth = NonEmpty.maximum estimateWidths
        refinedEstimates = NonEmpty.map (refineWiderThan (0.5 * maxWidth)) estimates
     in wrap (Sum refinedEstimates)

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
