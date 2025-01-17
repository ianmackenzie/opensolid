module OpenSolid.SurfaceLinearization (error) where

import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range
import OpenSolid.SurfaceParameter (UvBounds)

error :: UvBounds -> Range units -> Range units -> Range units -> Qty units
error subdomain fuu fuv fvv = do
  let Bounds2d uRange vRange = subdomain
  let uWidth = Range.width uRange
  let vWidth = Range.width vRange
  let uuDelta = 0.125 * fuu * Qty.squared uWidth
  let uvDelta = 0.25 * fuv * uWidth * vWidth
  let vvDelta = 0.125 * fvv * Qty.squared vWidth
  let uuError = Range.maxAbs uuDelta
  let vvError = Range.maxAbs vvDelta
  let uvError = Range.maxAbs (uuDelta + vvDelta + uvDelta)
  let vuError = Range.maxAbs (uuDelta + vvDelta - uvDelta)
  Qty.max (Qty.max (Qty.max uuError vvError) uvError) vuError
