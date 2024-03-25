module Curve1d.Integral
  ( Integral (Integral)
  )
where

import {-# SOURCE #-} Curve1d (Curve1d)
import {-# SOURCE #-} Curve1d qualified
import Estimate qualified
import OpenSolid
import Qty qualified
import Range (Range)
import Range qualified

data Integral units = Integral (Curve1d units) (Curve1d units) (Range Unitless)

instance Estimate.Interface (Integral units) units where
  boundsImpl (Integral curve derivative domain) =
    let dx = Range.width domain
        derivativeBounds = Curve1d.segmentBounds domain derivative
        estimate0 = dx * Curve1d.segmentBounds domain curve
        y1 = Curve1d.evaluateAt (Range.minValue domain) curve
        y2 = Curve1d.evaluateAt (Range.maxValue domain) curve
        m = Range.width derivativeBounds
        error1 = 0.125 * m * dx * dx
        estimate1 = dx * Qty.midpoint y1 y2 + Range.from -error1 error1
     in case Range.intersection estimate0 estimate1 of
          Just intersection -> intersection
          Nothing -> estimate0 -- Shouldn't happen if bounds are correct

  refineImpl (Integral curve derivative domain) =
    let (leftDomain, rightDomain) = Range.bisect domain
        leftIntegral = Integral curve derivative leftDomain
        rightIntegral = Integral curve derivative rightDomain
     in Estimate.wrap leftIntegral + Estimate.wrap rightIntegral
