module OpenSolid.Linearization (error) where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude

error :: Bounds units -> Bounds Unitless -> Quantity units
error secondDerivativeMagnitude subdomain = do
  let maxSecondDerivativeMagnitude = Bounds.maxAbs secondDerivativeMagnitude
  0.125 *. maxSecondDerivativeMagnitude .*. Number.squared (Bounds.width subdomain)
