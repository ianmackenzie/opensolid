module OpenSolid.VectorCurve2d
  ( Interface (..)
  , VectorCurve2d
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
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Curve1d (Curve1d)
import OpenSolid.Vector2d (Vector2d)
import Range (Range)
import Transform2d (Transform2d)
import Units qualified
import VectorBounds2d (VectorBounds2d)

class
  Show curve =>
  Interface curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  evaluateImpl :: curve -> Float -> Vector2d coordinateSystem
  evaluateBoundsImpl :: curve -> Range Unitless -> VectorBounds2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem
  transformByImpl ::
    Transform2d tag (Space coordinateSystem @ translationUnits) ->
    curve ->
    VectorCurve2d coordinateSystem

type role VectorCurve2d nominal

data VectorCurve2d (coordinateSystem :: CoordinateSystem)

instance Show (VectorCurve2d (space @ units))

instance Negation (VectorCurve2d (space @ units))

instance Multiplication' Sign (VectorCurve2d (space @ units))

instance Multiplication Sign (VectorCurve2d (space @ units)) (VectorCurve2d (space @ units))

instance Multiplication' (VectorCurve2d (space @ units)) Sign

instance Multiplication (VectorCurve2d (space @ units)) Sign (VectorCurve2d (space @ units))

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve2d (space1 @ unitsA)) (VectorCurve2d (space2 @ unitsB))

instance Multiplication' (Curve1d units1) (VectorCurve2d (space @ units2))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1d units1) (VectorCurve2d (space @ units2)) (VectorCurve2d (space @ units3))

instance Multiplication' (VectorCurve2d (space @ units1)) (Curve1d units2)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3))

instance Division' (VectorCurve2d (space @ units1)) (Curve1d units2)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3))

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorCurve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve1d units3)

constant :: Vector2d (space @ units) -> VectorCurve2d (space @ units)
new :: Interface curve (space @ units) => curve -> VectorCurve2d (space @ units)
evaluate :: VectorCurve2d (space @ units) -> Float -> Vector2d (space @ units)
evaluateBounds :: VectorCurve2d (space @ units) -> Range Unitless -> VectorBounds2d (space @ units)
derivative :: VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
unsafeMagnitude :: VectorCurve2d (space @ units) -> Curve1d units
transformBy ::
  Transform2d tag (space @ translationUnits) ->
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units)
