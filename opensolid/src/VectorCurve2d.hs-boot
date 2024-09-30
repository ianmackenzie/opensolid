module VectorCurve2d
  ( Interface (..)
  , VectorCurve2d
  , constant
  , new
  , evaluateAt
  , segmentBounds
  , derivative
  , unsafeMagnitude
  , transformBy
  , toAst
  )
where

import CoordinateSystem (Space)
import {-# SOURCE #-} Curve1d (Curve1d)
import Jit.Expression qualified as Expression
import Jit.VectorExpression2d (VectorExpression2d)
import OpenSolid
import Range (Range)
import Transform2d (Transform2d)
import Units qualified
import Vector2d (Vector2d)
import VectorBounds2d (VectorBounds2d)

class
  Show curve =>
  Interface curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  evaluateAtImpl :: Float -> curve -> Vector2d coordinateSystem
  segmentBoundsImpl :: Range Unitless -> curve -> VectorBounds2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem
  transformByImpl ::
    Transform2d tag (Space coordinateSystem @ translationUnits) ->
    curve ->
    VectorCurve2d coordinateSystem
  toAstImpl :: curve -> Maybe (VectorExpression2d Expression.Curve)

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
evaluateAt :: Float -> VectorCurve2d (space @ units) -> Vector2d (space @ units)
segmentBounds :: Range Unitless -> VectorCurve2d (space @ units) -> VectorBounds2d (space @ units)
derivative :: VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
unsafeMagnitude :: VectorCurve2d (space @ units) -> Curve1d units
transformBy ::
  Transform2d tag (space @ translationUnits) ->
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units)
toAst :: VectorCurve2d (space @ units) -> Maybe (VectorExpression2d Expression.Curve)
