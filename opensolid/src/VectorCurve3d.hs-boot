module VectorCurve3d
  ( Interface (..)
  , VectorCurve3d
  , constant
  , new
  , evaluate
  , evaluateBounds
  , derivative
  , unsafeMagnitude
  , transformBy
  )
where

import CoordinateSystem (Space)
import {-# SOURCE #-} Curve1d (Curve1d)
import OpenSolid
import Range (Range)
import Transform3d (Transform3d)
import Units qualified
import Vector3d (Vector3d)
import VectorBounds3d (VectorBounds3d)

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

data VectorCurve3d (coordinateSystem :: CoordinateSystem)

instance Show (VectorCurve3d (space @ units))

instance Negation (VectorCurve3d (space @ units))

instance Multiplication' Sign (VectorCurve3d (space @ units))

instance Multiplication Sign (VectorCurve3d (space @ units)) (VectorCurve3d (space @ units))

instance Multiplication' (VectorCurve3d (space @ units)) Sign

instance Multiplication (VectorCurve3d (space @ units)) Sign (VectorCurve3d (space @ units))

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve3d (space1 @ unitsA)) (VectorCurve3d (space2 @ unitsB))

instance Multiplication' (Curve1d units1) (VectorCurve3d (space @ units2))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1d units1) (VectorCurve3d (space @ units2)) (VectorCurve3d (space @ units3))

instance Multiplication' (VectorCurve3d (space @ units1)) (Curve1d units2)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3d (space @ units1)) (Curve1d units2) (VectorCurve3d (space @ units3))

instance Division' (VectorCurve3d (space @ units1)) (Curve1d units2)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve3d (space @ units1)) (Curve1d units2) (VectorCurve3d (space @ units3))

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve1d units3)

constant :: Vector3d (space @ units) -> VectorCurve3d (space @ units)
new :: Interface curve (space @ units) => curve -> VectorCurve3d (space @ units)
evaluate :: VectorCurve3d (space @ units) -> Float -> Vector3d (space @ units)
evaluateBounds :: VectorCurve3d (space @ units) -> Range Unitless -> VectorBounds3d (space @ units)
derivative :: VectorCurve3d (space @ units) -> VectorCurve3d (space @ units)
unsafeMagnitude :: VectorCurve3d (space @ units) -> Curve1d units
transformBy ::
  Transform3d tag (space @ translationUnits) ->
  VectorCurve3d (space @ units) ->
  VectorCurve3d (space @ units)
