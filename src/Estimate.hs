module Estimate
  ( Estimate (Estimate)
  , IsEstimate (..)
  , exact
  , bounds
  , refine
  , sum
  , min
  , max
  )
where

import NonEmpty qualified
import OpenSolid
import Qty qualified
import Range (Range)
import Range qualified

class IsEstimate a units | a -> units where
  boundsImpl :: a -> Range units
  refineImpl :: a -> Estimate units

instance IsEstimate (Qty units) units where
  boundsImpl = Range.constant
  refineImpl = exact

data Estimate units where
  Estimate :: IsEstimate a units => a -> Estimate units

exact :: Qty units -> Estimate units
exact value = Estimate value

bounds :: Estimate units -> Range units
bounds (Estimate estimate) = boundsImpl estimate

refine :: Estimate units -> Estimate units
refine (Estimate estimate) = refineImpl estimate

data Sum units = Sum (NonEmpty (Estimate units)) (Qty units) (Range units)

sum :: List (Estimate units) -> Estimate units
sum [] = exact Qty.zero
sum (NonEmpty estimates) = sumOf estimates

sumOf :: NonEmpty (Estimate units) -> Estimate units
sumOf estimates =
  let individualBounds = NonEmpty.map bounds estimates
      maxWidth = NonEmpty.maximum (NonEmpty.map Range.width individualBounds)
      boundsSum = NonEmpty.sum individualBounds
   in Estimate (Sum estimates maxWidth boundsSum)

instance IsEstimate (Sum units) units where
  boundsImpl (Sum _ _ boundsSum) = boundsSum
  refineImpl (Sum estimates maxWidth _) =
    sumOf (NonEmpty.map (refineWiderThan (0.5 * maxWidth)) estimates)

refineWiderThan :: Qty units -> Estimate units -> Estimate units
refineWiderThan desiredWidth estimate
  | Range.width (bounds estimate) > desiredWidth = refine estimate
  | otherwise = estimate

data Min units = Min (Estimate units) (Estimate units)

instance IsEstimate (Min units) units where
  boundsImpl (Min first second) = Range.min (bounds first) (bounds second)
  refineImpl (Min first second)
    | Range.maxValue firstBounds <= Range.minValue secondBounds = refine first
    | Range.maxValue secondBounds <= Range.minValue firstBounds = refine second
    | otherwise = min (refine first) (refine second)
   where
    firstBounds = bounds first
    secondBounds = bounds second

min :: Estimate units -> Estimate units -> Estimate units
min first second = Estimate (Min first second)

data Max units = Max (Estimate units) (Estimate units)

instance IsEstimate (Max units) units where
  boundsImpl (Max first second) = Range.max (bounds first) (bounds second)
  refineImpl (Max first second)
    | Range.minValue firstBounds >= Range.maxValue secondBounds = refine first
    | Range.minValue secondBounds >= Range.maxValue firstBounds = refine second
    | otherwise = max (refine first) (refine second)
   where
    firstBounds = bounds first
    secondBounds = bounds second

max :: Estimate units -> Estimate units -> Estimate units
max first second = Estimate (Max first second)
