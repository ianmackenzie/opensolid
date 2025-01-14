module OpenSolid.Linearization (error) where

import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range

error :: Range Unitless -> Range units -> Qty units
error subdomain secondDerivativeMagnitude = do
  let maxSecondDerivativeMagnitude = Range.maxAbs secondDerivativeMagnitude
  0.125 * maxSecondDerivativeMagnitude * Qty.squared (Range.width subdomain)
