module OpenSolid.VectorCurve3d
  ( Interface (..)
  , VectorCurve3d (Parametric, Transformed)
  , constant
  , new
  , planar
  , evaluate
  , evaluateBounds
  , derivative
  , unsafeMagnitude
  , transformBy
  )
where

import OpenSolid.Basis3d (Basis3d)
import OpenSolid.CoordinateSystem (Space)
import {-# SOURCE #-} OpenSolid.Curve (Curve)
import OpenSolid.Expression (Expression)
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.VectorBounds3d (VectorBounds3d)
import {-# SOURCE #-} OpenSolid.VectorCurve2d (VectorCurve2d)

class
  Show curve =>
  Interface curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  evaluateImpl :: curve -> Float -> Vector3d coordinateSystem
  evaluateBoundsImpl :: curve -> Range Unitless -> VectorBounds3d coordinateSystem
  derivativeImpl :: curve -> VectorCurve3d coordinateSystem
  transformByImpl ::
    Transform3d tag (Space coordinateSystem @ translationUnits) ->
    curve ->
    VectorCurve3d coordinateSystem

type role VectorCurve3d nominal

data VectorCurve3d (coordinateSystem :: CoordinateSystem) where
  VectorCurve3d ::
    Interface curve (space @ units) =>
    curve ->
    VectorCurve3d (space @ units)
  Parametric ::
    Expression Float (Vector3d (space @ units)) ->
    VectorCurve3d (space @ units)
  Coerce ::
    VectorCurve3d (space @ units1) ->
    VectorCurve3d (space @ units2)
  Reversed ::
    VectorCurve3d (space @ units) ->
    VectorCurve3d (space @ units)
  XYZ ::
    Curve units ->
    Curve units ->
    Curve units ->
    VectorCurve3d (space @ units)
  Negated ::
    VectorCurve3d (space @ units) ->
    VectorCurve3d (space @ units)
  Sum ::
    VectorCurve3d (space @ units) ->
    VectorCurve3d (space @ units) ->
    VectorCurve3d (space @ units)
  Difference ::
    VectorCurve3d (space @ units) ->
    VectorCurve3d (space @ units) ->
    VectorCurve3d (space @ units)
  Product1d3d' ::
    Curve units1 ->
    VectorCurve3d (space @ units2) ->
    VectorCurve3d (space @ (units1 :*: units2))
  Product3d1d' ::
    VectorCurve3d (space @ units1) ->
    Curve units2 ->
    VectorCurve3d (space @ (units1 :*: units2))
  Quotient' ::
    VectorCurve3d (space @ units1) ->
    Curve units2 ->
    VectorCurve3d (space @ (units1 :/: units2))
  CrossProduct' ::
    VectorCurve3d (space @ units1) ->
    VectorCurve3d (space @ units2) ->
    VectorCurve3d (space @ (units1 :*: units2))
  PlaceInBasis ::
    Basis3d global (Defines local) ->
    VectorCurve3d (local @ units) ->
    VectorCurve3d (global @ units)
  Transformed ::
    Transform3d tag (space @ translationUnits) ->
    VectorCurve3d (space @ units) ->
    VectorCurve3d (space @ units)
  Planar ::
    Plane3d (space @ originPointUnits) (Defines local) ->
    VectorCurve2d (local @ units) ->
    VectorCurve3d (space @ units)

instance HasUnits (VectorCurve3d (space @ units)) units (VectorCurve3d (space @ Unitless))

instance Show (VectorCurve3d (space @ units))

instance Negation (VectorCurve3d (space @ units))

instance Multiplication Sign (VectorCurve3d (space @ units)) (VectorCurve3d (space @ units))

instance Multiplication (VectorCurve3d (space @ units)) Sign (VectorCurve3d (space @ units))

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve3d (space1 @ unitsA)) (VectorCurve3d (space2 @ unitsB))

instance
  Multiplication'
    (Curve units1)
    (VectorCurve3d (space @ units2))
    (VectorCurve3d (space @ (units1 :*: units2)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (VectorCurve3d (space @ units2)) (VectorCurve3d (space @ units3))

instance
  Multiplication'
    (VectorCurve3d (space @ units1))
    (Curve units2)
    (VectorCurve3d (space @ (units1 :*: units2)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3d (space @ units1)) (Curve units2) (VectorCurve3d (space @ units3))

instance
  Division'
    (VectorCurve3d (space @ units1))
    (Curve units2)
    (VectorCurve3d (space @ (units1 :/: units2)))

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve3d (space @ units1)) (Curve units2) (VectorCurve3d (space @ units3))

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve (units1 :*: units2))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve units3)

constant :: Vector3d (space @ units) -> VectorCurve3d (space @ units)
new :: Interface curve (space @ units) => curve -> VectorCurve3d (space @ units)
planar ::
  Plane3d (space @ originPointUnits) (Defines local) ->
  VectorCurve2d (local @ units) ->
  VectorCurve3d (space @ units)
evaluate :: VectorCurve3d (space @ units) -> Float -> Vector3d (space @ units)
evaluateBounds :: VectorCurve3d (space @ units) -> Range Unitless -> VectorBounds3d (space @ units)
derivative :: VectorCurve3d (space @ units) -> VectorCurve3d (space @ units)
unsafeMagnitude :: VectorCurve3d (space @ units) -> Curve units
transformBy ::
  Transform3d tag (space @ translationUnits) ->
  VectorCurve3d (space @ units) ->
  VectorCurve3d (space @ units)
