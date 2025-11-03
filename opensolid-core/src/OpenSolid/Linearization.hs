module OpenSolid.Linearization (error) where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Float qualified as Float
import OpenSolid.Prelude

error :: Bounds units -> Bounds Unitless -> Quantity units
error secondDerivativeMagnitude subdomain = do
  let maxSecondDerivativeMagnitude = Bounds.maxAbs secondDerivativeMagnitude
  0.125 * maxSecondDerivativeMagnitude * Float.squared (Bounds.width subdomain)
