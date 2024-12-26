module OpenSolid.Curve1d.Integral
  ( Integral (Integral)
  )
where

import {-# SOURCE #-} OpenSolid.Curve1d (Curve1d)
import {-# SOURCE #-} OpenSolid.Curve1d qualified as Curve1d
import OpenSolid.Estimate qualified as Estimate
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range

data Integral units = Integral (Curve1d units) (Curve1d units) (Range Unitless)

instance Estimate.Interface (Integral units) units where
  boundsImpl (Integral curve derivative domain) = do
    let dx = Range.width domain
    let derivativeBounds = Curve1d.evaluateBounds derivative domain
    let estimate0 = dx * Curve1d.evaluateBounds curve domain
    let y1 = Curve1d.evaluate curve (Range.lowerBound domain)
    let y2 = Curve1d.evaluate curve (Range.upperBound domain)
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
