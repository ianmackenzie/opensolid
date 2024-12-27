module OpenSolid.Curve1d.Function.Integral
  ( Integral (Integral)
  )
where

import {-# SOURCE #-} OpenSolid.Curve1d.Function (Function)
import {-# SOURCE #-} OpenSolid.Curve1d.Function qualified as Function
import OpenSolid.Estimate qualified as Estimate
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range

data Integral units = Integral (Function units) (Function units) (Range Unitless)

instance Estimate.Interface (Integral units) units where
  boundsImpl (Integral function derivative domain) = do
    let dx = Range.width domain
    let derivativeBounds = Function.evaluateBounds derivative domain
    let estimate0 = dx * Function.evaluateBounds function domain
    let y1 = Function.evaluate function (Range.lowerBound domain)
    let y2 = Function.evaluate function (Range.upperBound domain)
    let m = Range.width derivativeBounds
    let error1 = 0.125 * m * dx * dx
    let estimate1 = dx * Qty.midpoint y1 y2 + Range.from -error1 error1
    case Range.intersection estimate0 estimate1 of
      Just intersection -> intersection
      Nothing -> estimate0 -- Shouldn't happen if bounds are correct

  refineImpl (Integral curve derivative domain) = do
    let (leftDomain, rightDomain) = Range.bisect domain
    let leftIntegral = Integral curve derivative leftDomain
    let rightIntegral = Integral curve derivative rightDomain
    Estimate.new leftIntegral + Estimate.new rightIntegral
