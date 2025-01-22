module OpenSolid.Linearization (maxDomainSize) where

import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range

maxDomainSize :: Qty units -> Range units -> Float
maxDomainSize accuracy secondDerivativeMagnitude = do
  let maxSecondDerivativeMagnitude = Range.maxAbs secondDerivativeMagnitude
  if maxSecondDerivativeMagnitude == Qty.zero
    then Qty.infinity
    else Qty.sqrt (8.0 * accuracy / maxSecondDerivativeMagnitude)
