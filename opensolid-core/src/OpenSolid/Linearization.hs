module OpenSolid.Linearization (error) where

import OpenSolid.Float qualified as Float
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range

error :: Range units -> Range Unitless -> Qty units
error secondDerivativeMagnitude subdomain = do
  let maxSecondDerivativeMagnitude = Range.maxAbs secondDerivativeMagnitude
  0.125 * maxSecondDerivativeMagnitude * Float.squared (Range.width subdomain)
