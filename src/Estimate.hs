module Estimate
  ( Estimate (Estimate)
  , IsEstimate (..)
  , bounds
  , refine
  )
where

import Range (Range)

class IsEstimate a units | a -> units where
  boundsImpl :: a -> Range units
  refineImpl :: a -> a

data Estimate units where
  Estimate :: IsEstimate a units => a -> Estimate units

bounds :: Estimate units -> Range units
bounds (Estimate estimate) = boundsImpl estimate

refine :: Estimate units -> Estimate units
refine (Estimate estimate) = Estimate (refineImpl estimate)
