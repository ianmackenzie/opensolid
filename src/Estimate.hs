module Estimate
  ( Estimate (Estimate)
  , IsEstimate (..)
  , exact
  , bounds
  , refine
  , sum
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
