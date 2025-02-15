module OpenSolid.Curve3d
  ( Curve3d (Parametric)
  , Interface (..)
  , constant
  , new
  , planar
  , evaluate
  , evaluateBounds
  , derivative
  , reverse
  )
where

import OpenSolid.Bounds3d (Bounds3d)
import OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Expression (Expression)
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Transform3d (Transform3d)
import {-# SOURCE #-} OpenSolid.VectorCurve3d (VectorCurve3d)

type role Curve3d nominal

data Curve3d (coordinateSystem :: CoordinateSystem) where
  Curve ::
    Interface curve (space @ units) =>
    curve ->
    Curve3d (space @ units)
  Parametric ::
    Expression Float (Point3d (space @ units)) ->
    Curve3d (space @ units)
  Coerce ::
    Curve3d (space @ units1) ->
    Curve3d (space @ units2)
  XYZ ::
    Curve units ->
    Curve units ->
    Curve units ->
    Curve3d (space @ units)
  Addition ::
    Curve3d (space @ units) ->
    VectorCurve3d (space @ units) ->
    Curve3d (space @ units)
  Subtraction ::
    Curve3d (space @ units) ->
    VectorCurve3d (space @ units) ->
    Curve3d (space @ units)
  Transformed ::
    Transform3d tag (space @ units) ->
    Curve3d (space @ units) ->
    Curve3d (space @ units)
  Planar ::
    Plane3d (space @ units) (Defines local) ->
    Curve2d (local @ units) ->
    Curve3d (space @ units)

instance Show (Curve3d (space @ units))

class
  Show curve =>
  Interface curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  evaluateImpl :: curve -> Float -> Point3d coordinateSystem
  evaluateBoundsImpl :: curve -> Range Unitless -> Bounds3d coordinateSystem
  derivativeImpl :: curve -> VectorCurve3d coordinateSystem
  reverseImpl :: curve -> Curve3d coordinateSystem
  transformByImpl :: Transform3d tag coordinateSystem -> curve -> Curve3d coordinateSystem

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
new :: Interface curve (space @ units) => curve -> Curve3d (space @ units)
planar ::
  Plane3d (space @ units) (Defines local) ->
  Curve2d (local @ units) ->
  Curve3d (space @ units)
evaluate :: Curve3d (space @ units) -> Float -> Point3d (space @ units)
evaluateBounds :: Curve3d (space @ units) -> Range Unitless -> Bounds3d (space @ units)
derivative :: Curve3d (space @ units) -> VectorCurve3d (space @ units)
reverse :: Curve3d (space @ units) -> Curve3d (space @ units)
