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
  boundsImpl (Integral curve derivative domain) = do
    let dx = Range.width domain
    let derivativeBounds = Curve1d.evaluateBounds derivative domain
    let estimate0 = dx * Curve1d.evaluateBounds curve domain
    let y1 = Curve1d.evaluate curve (Range.minValue domain)
    let y2 = Curve1d.evaluate curve (Range.maxValue domain)
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
