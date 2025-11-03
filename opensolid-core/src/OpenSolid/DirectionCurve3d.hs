module OpenSolid.DirectionCurve3d
  ( DirectionCurve3d
  , unsafe
  , unwrap
  , startValue
  , endValue
  , evaluate
  , evaluateBounds
  , derivative
  , constant
  , reverse
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Curve (Curve)
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.DirectionBounds3d (DirectionBounds3d)
import OpenSolid.DirectionBounds3d qualified as DirectionBounds3d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Prelude
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorCurve3d (VectorCurve3d)
import OpenSolid.VectorCurve3d qualified as VectorCurve3d

newtype DirectionCurve3d space = DirectionCurve3d (VectorCurve3d (space @ Unitless))

unsafe :: VectorCurve3d (space @ Unitless) -> DirectionCurve3d space
unsafe = DirectionCurve3d

unwrap :: DirectionCurve3d space -> VectorCurve3d (space @ Unitless)
unwrap (DirectionCurve3d vectorCurve) = vectorCurve

startValue :: DirectionCurve3d space -> Direction3d space
startValue curve = evaluate curve 0.0

endValue :: DirectionCurve3d space -> Direction3d space
endValue curve = evaluate curve 1.0

evaluate :: DirectionCurve3d space -> Float -> Direction3d space
evaluate (DirectionCurve3d vectorCurve) tValue =
  Direction3d.unsafe (VectorCurve3d.evaluate vectorCurve tValue)

evaluateBounds :: DirectionCurve3d space -> Bounds Unitless -> DirectionBounds3d space
evaluateBounds (DirectionCurve3d vectorCurve) tBounds =
  DirectionBounds3d.unsafe (VectorCurve3d.evaluateBounds vectorCurve tBounds)

derivative :: DirectionCurve3d space -> VectorCurve3d (space @ Unitless)
derivative (DirectionCurve3d vectorCurve) = vectorCurve.derivative

constant :: Direction3d space -> DirectionCurve3d space
constant direction = DirectionCurve3d (VectorCurve3d.constant (Vector3d.unit direction))

reverse :: DirectionCurve3d space -> DirectionCurve3d space
reverse (DirectionCurve3d vectorCurve) = DirectionCurve3d (VectorCurve3d.reverse vectorCurve)

instance Negation (DirectionCurve3d space) where
  negate (DirectionCurve3d vectorCurve) = DirectionCurve3d (negate vectorCurve)

instance Multiplication Sign (DirectionCurve3d space) (DirectionCurve3d space) where
  Positive * curve = curve
  Negative * curve = -curve

instance Multiplication' Sign (DirectionCurve3d space) (DirectionCurve3d space) where
  Positive .*. curve = curve
  Negative .*. curve = -curve

instance Multiplication (DirectionCurve3d space) Sign (DirectionCurve3d space) where
  curve * Positive = curve
  curve * Negative = -curve

instance Multiplication' (DirectionCurve3d space) Sign (DirectionCurve3d space) where
  curve .*. Positive = curve
  curve .*. Negative = -curve

instance Multiplication (Qty units) (DirectionCurve3d space) (VectorCurve3d (space @ units)) where
  value * DirectionCurve3d vectorCurve = value * vectorCurve

instance Multiplication (DirectionCurve3d space) (Qty units) (VectorCurve3d (space @ units)) where
  DirectionCurve3d vectorCurve * value = vectorCurve * value

instance
  Multiplication
    (Curve units)
    (DirectionCurve3d space)
    (VectorCurve3d (space @ units))
  where
  scalarCurve * DirectionCurve3d vectorCurve = scalarCurve * vectorCurve

instance
  Multiplication
    (DirectionCurve3d space)
    (Curve units)
    (VectorCurve3d (space @ units))
  where
  DirectionCurve3d vectorCurve * scalarCurve = vectorCurve * scalarCurve

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve3d space1) (DirectionCurve3d space2) (Curve Unitless)
  where
  DirectionCurve3d lhs `dot` DirectionCurve3d rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve3d space1) (VectorCurve3d (space2 @ units)) (Curve units)
  where
  DirectionCurve3d lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (VectorCurve3d (space1 @ units)) (DirectionCurve3d space2) (Curve units)
  where
  lhs `dot` DirectionCurve3d rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve3d space1) (Direction3d space2) (Curve Unitless)
  where
  DirectionCurve3d lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (Direction3d space1) (DirectionCurve3d space2) (Curve Unitless)
  where
  lhs `dot` DirectionCurve3d rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve3d space1) (Vector3d (space2 @ units)) (Curve units)
  where
  DirectionCurve3d lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (Vector3d (space1 @ units)) (DirectionCurve3d space2) (Curve units)
  where
  lhs `dot` DirectionCurve3d rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionCurve3d space1)
    (DirectionCurve3d space2)
    (VectorCurve3d (space1 @ Unitless))
  where
  DirectionCurve3d lhs `cross` DirectionCurve3d rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionCurve3d space1)
    (VectorCurve3d (space2 @ units))
    (VectorCurve3d (space1 @ units))
  where
  DirectionCurve3d lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorCurve3d (space1 @ units))
    (DirectionCurve3d space2)
    (VectorCurve3d (space1 @ units))
  where
  lhs `cross` DirectionCurve3d rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionCurve3d space1)
    (Direction3d space2)
    (VectorCurve3d (space1 @ Unitless))
  where
  DirectionCurve3d lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3d space1)
    (DirectionCurve3d space2)
    (VectorCurve3d (space1 @ Unitless))
  where
  lhs `cross` DirectionCurve3d rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionCurve3d space1)
    (Vector3d (space2 @ units))
    (VectorCurve3d (space1 @ units))
  where
  DirectionCurve3d lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Vector3d (space1 @ units))
    (DirectionCurve3d space2)
    (VectorCurve3d (space1 @ units))
  where
  lhs `cross` DirectionCurve3d rhs = lhs `cross` rhs

instance Composition (Curve Unitless) (DirectionCurve3d space) (DirectionCurve3d space) where
  DirectionCurve3d curve . curve1d = DirectionCurve3d (curve . curve1d)

placeIn ::
  Frame3d (global @ frameUnits) (Defines local) ->
  DirectionCurve3d local ->
  DirectionCurve3d global
placeIn frame (DirectionCurve3d curve) =
  DirectionCurve3d (VectorCurve3d.placeIn frame curve)

relativeTo ::
  Frame3d (global @ frameUnits) (Defines local) ->
  DirectionCurve3d global ->
  DirectionCurve3d local
relativeTo frame = placeIn (Frame3d.inverse frame)
