module OpenSolid.SurfaceLinearization (error) where

import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Float qualified as Float
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.SurfaceParameter (UvBounds)
import OpenSolid.VectorBounds3d (VectorBounds3d)
import OpenSolid.VectorBounds3d qualified as VectorBounds3d

error ::
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units) ->
  UvBounds ->
  Qty units
error fuu fuv fvv subdomain = do
  let Bounds2d uBounds vBounds = subdomain
  let uWidthSquared = Float.squared (Bounds.width uBounds)
  let vWidthSquared = Float.squared (Bounds.width vBounds)
  let uvWidthSquared = uWidthSquared + vWidthSquared
  let uuMagnitude = Bounds.maxAbs (VectorBounds3d.magnitude fuu)
  let vvMagnitude = Bounds.maxAbs (VectorBounds3d.magnitude fvv)
  let uvMagnitude1 = Bounds.maxAbs (VectorBounds3d.magnitude (fuu + fvv + 2.0 * fuv))
  let uvMagnitude2 = Bounds.maxAbs (VectorBounds3d.magnitude (fuu + fvv - 2.0 * fuv))
  let uuError = 0.125 * uuMagnitude * uWidthSquared
  let vvError = 0.125 * vvMagnitude * vWidthSquared
  let uvError1 = 0.125 * uvMagnitude1 * uvWidthSquared
  let uvError2 = 0.125 * uvMagnitude2 * uvWidthSquared
  Qty.max (Qty.max uuError vvError) (Qty.max uvError1 uvError2)
