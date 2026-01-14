module OpenSolid.Linearization (error) where

import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude

error :: Interval units -> Interval Unitless -> Quantity units
error secondDerivativeMagnitude subdomain = do
  let maxSecondDerivativeMagnitude = Interval.maxAbs secondDerivativeMagnitude
  0.125 *. maxSecondDerivativeMagnitude .*. Number.squared (Interval.width subdomain)
