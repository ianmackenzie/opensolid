module OpenSolid.Curve3d
  ( Curve3d
  , Compiled
  , constant
  , new
  , on
  , evaluate
  , evaluateBounds
  , derivative
  , reverse
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds3d (Bounds3d)
import OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorCurve3d (VectorCurve3d)

type role Curve3d nominal

data Curve3d (coordinateSystem :: CoordinateSystem)

type Compiled (coordinateSystem :: CoordinateSystem) =
  CompiledFunction
    Float
    (Point3d coordinateSystem)
    (Bounds Unitless)
    (Bounds3d coordinateSystem)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Curve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve3d (space1 @ units1))

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve3d (space1 @ units1))

constant :: Point3d (space @ units) -> Curve3d (space @ units)
new :: Compiled (space @ units) -> VectorCurve3d (space @ units) -> Curve3d (space @ units)
on ::
  Plane3d (space @ units) (Defines local) ->
  Curve2d (local @ units) ->
  Curve3d (space @ units)
derivative :: Curve3d (space @ units) -> VectorCurve3d (space @ units)
evaluate :: Curve3d (space @ units) -> Float -> Point3d (space @ units)
evaluateBounds :: Curve3d (space @ units) -> Bounds Unitless -> Bounds3d (space @ units)
reverse :: Curve3d (space @ units) -> Curve3d (space @ units)
