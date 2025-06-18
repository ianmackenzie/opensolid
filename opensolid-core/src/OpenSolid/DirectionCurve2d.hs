module OpenSolid.DirectionCurve2d
  ( DirectionCurve2d
  , unsafe
  , unwrap
  , startValue
  , endValue
  , evaluate
  , evaluateBounds
  , derivative
  , constant
  , arc
  , reverse
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Bounds (Bounds)
import OpenSolid.Curve (Curve)
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.DirectionBounds2d (DirectionBounds2d)
import OpenSolid.DirectionBounds2d qualified as DirectionBounds2d
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

newtype DirectionCurve2d space = DirectionCurve2d (VectorCurve2d (space @ Unitless))

instance HasUnits (DirectionCurve2d space) Unitless (DirectionCurve2d space)

instance
  space1 ~ space2 =>
  Units.Coercion (DirectionCurve2d space1) (DirectionCurve2d space2)
  where
  coerce = identity

unsafe :: VectorCurve2d (space @ Unitless) -> DirectionCurve2d space
unsafe = DirectionCurve2d

unwrap :: DirectionCurve2d space -> VectorCurve2d (space @ Unitless)
unwrap (DirectionCurve2d vectorCurve) = vectorCurve

startValue :: DirectionCurve2d space -> Direction2d space
startValue curve = evaluate curve 0.0

endValue :: DirectionCurve2d space -> Direction2d space
endValue curve = evaluate curve 1.0

evaluate :: DirectionCurve2d space -> Float -> Direction2d space
evaluate (DirectionCurve2d vectorCurve) tValue =
  Direction2d.unsafe (VectorCurve2d.evaluate vectorCurve tValue)

evaluateBounds :: DirectionCurve2d space -> Bounds Unitless -> DirectionBounds2d space
evaluateBounds (DirectionCurve2d vectorCurve) tBounds =
  DirectionBounds2d.unsafe (VectorCurve2d.evaluateBounds vectorCurve tBounds)

derivative :: DirectionCurve2d space -> VectorCurve2d (space @ Unitless)
derivative (DirectionCurve2d vectorCurve) = vectorCurve.derivative

constant :: Direction2d space -> DirectionCurve2d space
constant direction = DirectionCurve2d (VectorCurve2d.constant (Vector2d.unit direction))

arc :: Angle -> Angle -> DirectionCurve2d space
arc a b = DirectionCurve2d (VectorCurve2d.arc (Vector2d 1.0 0.0) (Vector2d 0.0 1.0) a b)

reverse :: DirectionCurve2d space -> DirectionCurve2d space
reverse (DirectionCurve2d vectorCurve) = DirectionCurve2d (VectorCurve2d.reverse vectorCurve)

instance Negation (DirectionCurve2d space) where
  negate (DirectionCurve2d vectorCurve) = DirectionCurve2d (negate vectorCurve)

instance Multiplication Sign (DirectionCurve2d space) (DirectionCurve2d space) where
  Positive * curve = curve
  Negative * curve = -curve

instance Multiplication' Sign (DirectionCurve2d space) (DirectionCurve2d space) where
  Positive .*. curve = curve
  Negative .*. curve = -curve

instance Multiplication (DirectionCurve2d space) Sign (DirectionCurve2d space) where
  curve * Positive = curve
  curve * Negative = -curve

instance Multiplication' (DirectionCurve2d space) Sign (DirectionCurve2d space) where
  curve .*. Positive = curve
  curve .*. Negative = -curve

instance Multiplication (Qty units) (DirectionCurve2d space) (VectorCurve2d (space @ units)) where
  value * DirectionCurve2d vectorCurve = value * vectorCurve

instance Multiplication (DirectionCurve2d space) (Qty units) (VectorCurve2d (space @ units)) where
  DirectionCurve2d vectorCurve * value = vectorCurve * value

instance
  Multiplication
    (Curve units)
    (DirectionCurve2d space)
    (VectorCurve2d (space @ units))
  where
  scalarCurve * DirectionCurve2d vectorCurve = scalarCurve * vectorCurve

instance
  Multiplication
    (DirectionCurve2d space)
    (Curve units)
    (VectorCurve2d (space @ units))
  where
  DirectionCurve2d vectorCurve * scalarCurve = vectorCurve * scalarCurve

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve2d space1) (DirectionCurve2d space2) (Curve Unitless)
  where
  DirectionCurve2d lhs `dot` DirectionCurve2d rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve2d space1) (VectorCurve2d (space2 @ units)) (Curve units)
  where
  DirectionCurve2d lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (VectorCurve2d (space1 @ units)) (DirectionCurve2d space2) (Curve units)
  where
  lhs `dot` DirectionCurve2d rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve2d space1) (Direction2d space2) (Curve Unitless)
  where
  DirectionCurve2d lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2d space1) (DirectionCurve2d space2) (Curve Unitless)
  where
  lhs `dot` DirectionCurve2d rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve2d space1) (Vector2d (space2 @ units)) (Curve units)
  where
  DirectionCurve2d lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (Vector2d (space1 @ units)) (DirectionCurve2d space2) (Curve units)
  where
  lhs `dot` DirectionCurve2d rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionCurve2d space1) (DirectionCurve2d space2) (Curve Unitless)
  where
  DirectionCurve2d lhs `cross` DirectionCurve2d rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionCurve2d space1) (VectorCurve2d (space2 @ units)) (Curve units)
  where
  DirectionCurve2d lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication (VectorCurve2d (space1 @ units)) (DirectionCurve2d space2) (Curve units)
  where
  lhs `cross` DirectionCurve2d rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionCurve2d space1) (Direction2d space2) (Curve Unitless)
  where
  DirectionCurve2d lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2d space1) (DirectionCurve2d space2) (Curve Unitless)
  where
  lhs `cross` DirectionCurve2d rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionCurve2d space1) (Vector2d (space2 @ units)) (Curve units)
  where
  DirectionCurve2d lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication (Vector2d (space1 @ units)) (DirectionCurve2d space2) (Curve units)
  where
  lhs `cross` DirectionCurve2d rhs = lhs `cross` rhs

instance Composition (Curve Unitless) (DirectionCurve2d space) (DirectionCurve2d space) where
  curve1d >> DirectionCurve2d curve = DirectionCurve2d (curve1d >> curve)

instance HasField "xComponent" (DirectionCurve2d space) (Curve Unitless) where
  getField (DirectionCurve2d curve) = curve.xComponent

instance HasField "yComponent" (DirectionCurve2d space) (Curve Unitless) where
  getField (DirectionCurve2d curve) = curve.yComponent

placeIn ::
  Frame2d (global @ frameUnits) (Defines local) ->
  DirectionCurve2d local ->
  DirectionCurve2d global
placeIn frame (DirectionCurve2d curve) = DirectionCurve2d (VectorCurve2d.placeIn frame curve)

relativeTo ::
  Frame2d (global @ frameUnits) (Defines local) ->
  DirectionCurve2d global ->
  DirectionCurve2d local
relativeTo frame = placeIn (Frame2d.inverse frame)
