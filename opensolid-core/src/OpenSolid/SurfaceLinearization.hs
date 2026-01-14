module OpenSolid.SurfaceLinearization (error) where

import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Prelude
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Unboxed.Math
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorBounds3D qualified as VectorBounds3D

error ::
  VectorBounds3D units space ->
  VectorBounds3D units space ->
  VectorBounds3D units space ->
  UvBounds ->
  Quantity units
error fuu fuv fvv subdomain = Quantity# (error# fuu fuv fvv subdomain)

error# ::
  VectorBounds3D units space ->
  VectorBounds3D units space ->
  VectorBounds3D units space ->
  UvBounds ->
  Double#
error# fuu fuv fvv (Bounds2D uBounds vBounds) = do
  let uWidthSquared# = squared# (Interval.width# uBounds)
  let vWidthSquared# = squared# (Interval.width# vBounds)
  let uvWidthSquared# = uWidthSquared# +# vWidthSquared#
  let uuMagnitude# = VectorBounds3D.maxMagnitude# fuu
  let vvMagnitude# = VectorBounds3D.maxMagnitude# fvv
  let fA = fuu .+. fvv
  let fB = 2 *. fuv
  let uvMagnitude1# = VectorBounds3D.maxMagnitude# (fA .+. fB)
  let uvMagnitude2# = VectorBounds3D.maxMagnitude# (fA .-. fB)
  let uuError# = 0.125## *# uuMagnitude# *# uWidthSquared#
  let vvError# = 0.125## *# vvMagnitude# *# vWidthSquared#
  let uvError1# = 0.125## *# uvMagnitude1# *# uvWidthSquared#
  let uvError2# = 0.125## *# uvMagnitude2# *# uvWidthSquared#
  max# (max# uuError# vvError#) (max# uvError1# uvError2#)
