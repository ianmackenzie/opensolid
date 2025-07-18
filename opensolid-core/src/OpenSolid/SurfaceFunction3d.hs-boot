module OpenSolid.SurfaceFunction3d
  ( SurfaceFunction3d
  , Compiled
  , constant
  , evaluate
  , evaluateBounds
  , derivative
  , transformBy
  )
where

import OpenSolid.Bounds3d (Bounds3d)
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)

type role SurfaceFunction3d nominal

type SurfaceFunction3d :: CoordinateSystem -> Type
data SurfaceFunction3d coordinateSystem

instance
  HasField
    "du"
    (SurfaceFunction3d (space @ units))
    (VectorSurfaceFunction3d (space @ units))

instance
  HasField
    "dv"
    (SurfaceFunction3d (space @ units))
    (VectorSurfaceFunction3d (space @ units))

type Compiled coordinateSystem =
  CompiledFunction
    UvPoint
    (Point3d coordinateSystem)
    UvBounds
    (Bounds3d coordinateSystem)

instance HasField "compiled" (SurfaceFunction3d (space @ units)) (Compiled (space @ units))

instance HasUnits (SurfaceFunction3d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (SurfaceFunction3d (space1 @ unitsA)) (SurfaceFunction3d (space2 @ unitsB))

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (SurfaceFunction3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (SurfaceFunction3d (space1 @ units1))

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (SurfaceFunction3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (SurfaceFunction3d (space1 @ units1))

constant :: Point3d (space @ units) -> SurfaceFunction3d (space @ units)
evaluate :: SurfaceFunction3d (space @ units) -> UvPoint -> Point3d (space @ units)
evaluateBounds :: SurfaceFunction3d (space @ units) -> UvBounds -> Bounds3d (space @ units)
derivative ::
  SurfaceParameter ->
  SurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units)
transformBy ::
  Transform3d tag (space @ units) ->
  SurfaceFunction3d (space @ units) ->
  SurfaceFunction3d (space @ units)
