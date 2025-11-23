module OpenSolid.SurfaceLinearization (error) where

import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Prelude
import OpenSolid.Quantity (Quantity (Quantity##))
import OpenSolid.Unboxed.Math
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.VectorBounds3d (VectorBounds3d)
import OpenSolid.VectorBounds3d qualified as VectorBounds3d

error ::
  VectorBounds3d space units ->
  VectorBounds3d space units ->
  VectorBounds3d space units ->
  UvBounds ->
  Quantity units
error fuu fuv fvv subdomain = Quantity## (error## fuu fuv fvv subdomain)

error## ::
  VectorBounds3d space units ->
  VectorBounds3d space units ->
  VectorBounds3d space units ->
  UvBounds ->
  Double#
error## fuu fuv fvv (Bounds2d uBounds vBounds) = do
  let uWidthSquared## = squared## (Bounds.width## uBounds)
  let vWidthSquared## = squared## (Bounds.width## vBounds)
  let uvWidthSquared## = uWidthSquared## +## vWidthSquared##
  let uuMagnitude## = VectorBounds3d.maxMagnitude## fuu
  let vvMagnitude## = VectorBounds3d.maxMagnitude## fvv
  let fA = fuu .+. fvv
  let fB = 2 *. fuv
  let uvMagnitude1## = VectorBounds3d.maxMagnitude## (fA .+. fB)
  let uvMagnitude2## = VectorBounds3d.maxMagnitude## (fA .-. fB)
  let uuError## = 0.125## *## uuMagnitude## *## uWidthSquared##
  let vvError## = 0.125## *## vvMagnitude## *## vWidthSquared##
  let uvError1## = 0.125## *## uvMagnitude1## *## uvWidthSquared##
  let uvError2## = 0.125## *## uvMagnitude2## *## uvWidthSquared##
  max## (max## uuError## vvError##) (max## uvError1## uvError2##)
