module Estimate
  ( Estimate (Estimate)
  , IsEstimate (..)
  , exact
  , bounds
  , refine
  )
where

import OpenSolid
import Range (Range)
import Range qualified

class IsEstimate a units | a -> units where
  boundsImpl :: a -> Range units
  refineImpl :: a -> a

instance IsEstimate (Qty units) units where
  boundsImpl = Range.constant
  refineImpl = identity

data Estimate units where
  Estimate :: IsEstimate a units => a -> Estimate units

exact :: Qty units -> Estimate units
exact value = Estimate value

bounds :: Estimate units -> Range units
bounds (Estimate estimate) = boundsImpl estimate

refine :: Estimate units -> Estimate units
refine (Estimate estimate) = Estimate (refineImpl estimate)
