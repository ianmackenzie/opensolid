module OpenSolid.Curve3d
  ( Curve3d (Parametric)
  , Interface (..)
  , constant
  , new
  , evaluate
  , evaluateBounds
  , derivative
  , reverse
  )
where

import OpenSolid.Bounds3d (Bounds3d)
import OpenSolid.Curve (Curve)
import OpenSolid.Expression (Expression)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import {-# SOURCE #-} OpenSolid.VectorCurve3d (VectorCurve3d)

type role Curve3d nominal

data Curve3d (coordinateSystem :: CoordinateSystem) where
  Curve ::
    Interface function (space @ units) =>
    function ->
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

instance Show (Curve3d (space @ units))

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> Float -> Point3d coordinateSystem
  evaluateBoundsImpl :: function -> Range Unitless -> Bounds3d coordinateSystem
  derivativeImpl :: function -> VectorCurve3d coordinateSystem
  reverseImpl :: function -> Curve3d coordinateSystem

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
evaluate :: Curve3d (space @ units) -> Float -> Point3d (space @ units)
evaluateBounds :: Curve3d (space @ units) -> Range Unitless -> Bounds3d (space @ units)
derivative :: Curve3d (space @ units) -> VectorCurve3d (space @ units)
reverse :: Curve3d (space @ units) -> Curve3d (space @ units)
