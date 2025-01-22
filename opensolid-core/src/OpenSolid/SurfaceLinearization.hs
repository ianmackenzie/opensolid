module OpenSolid.SurfaceLinearization (maxDomainSize) where

import OpenSolid.Linearization qualified as Linearization
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range)

maxDomainSize :: Qty units -> Range units -> Range units -> Range units -> Float
maxDomainSize accuracy fuu fuv fvv = do
  let uuSize = Linearization.maxDomainSize accuracy fuu
  let vvSize = Linearization.maxDomainSize accuracy fvv
  let uvSize = Linearization.maxDomainSize accuracy (fuu + fvv - 2.0 * fuv)
  let vuSize = Linearization.maxDomainSize accuracy (fuu + fvv + 2.0 * fuv)
  Qty.min (Qty.min (Qty.min uuSize vvSize) uvSize) vuSize
