module OpenSolid.DirectionCurve3D
  ( DirectionCurve3D
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

import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Direction3D qualified as Direction3D
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.DirectionBounds3D qualified as DirectionBounds3D
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorCurve3D (VectorCurve3D)
import OpenSolid.VectorCurve3D qualified as VectorCurve3D

newtype DirectionCurve3D space = DirectionCurve3D (VectorCurve3D Unitless space)

unsafe :: VectorCurve3D Unitless space -> DirectionCurve3D space
unsafe = DirectionCurve3D

unwrap :: DirectionCurve3D space -> VectorCurve3D Unitless space
unwrap (DirectionCurve3D vectorCurve) = vectorCurve

startValue :: DirectionCurve3D space -> Direction3D space
startValue curve = evaluate curve 0.0

endValue :: DirectionCurve3D space -> Direction3D space
endValue curve = evaluate curve 1.0

evaluate :: DirectionCurve3D space -> Number -> Direction3D space
evaluate (DirectionCurve3D vectorCurve) tValue =
  Direction3D.unsafe (VectorCurve3D.evaluate vectorCurve tValue)

evaluateBounds :: DirectionCurve3D space -> Interval Unitless -> DirectionBounds3D space
evaluateBounds (DirectionCurve3D vectorCurve) tBounds =
  DirectionBounds3D.unsafe (VectorCurve3D.evaluateBounds vectorCurve tBounds)

derivative :: DirectionCurve3D space -> VectorCurve3D Unitless space
derivative (DirectionCurve3D vectorCurve) = VectorCurve3D.derivative vectorCurve

constant :: Direction3D space -> DirectionCurve3D space
constant direction = DirectionCurve3D (VectorCurve3D.constant (Vector3D.unit direction))

reverse :: DirectionCurve3D space -> DirectionCurve3D space
reverse (DirectionCurve3D vectorCurve) = DirectionCurve3D (VectorCurve3D.reverse vectorCurve)

instance Negation (DirectionCurve3D space) where
  negate (DirectionCurve3D vectorCurve) = DirectionCurve3D (negate vectorCurve)

instance Multiplication Sign (DirectionCurve3D space) (DirectionCurve3D space) where
  Positive * curve = curve
  Negative * curve = -curve

instance Multiplication_ Sign (DirectionCurve3D space) (DirectionCurve3D space) where
  Positive ?*? curve = curve
  Negative ?*? curve = -curve

instance Multiplication (DirectionCurve3D space) Sign (DirectionCurve3D space) where
  curve * Positive = curve
  curve * Negative = -curve

instance Multiplication_ (DirectionCurve3D space) Sign (DirectionCurve3D space) where
  curve ?*? Positive = curve
  curve ?*? Negative = -curve

instance
  Multiplication
    (Quantity units)
    (DirectionCurve3D space)
    (VectorCurve3D units space)
  where
  value * DirectionCurve3D vectorCurve = value * vectorCurve

instance
  Multiplication
    (DirectionCurve3D space)
    (Quantity units)
    (VectorCurve3D units space)
  where
  DirectionCurve3D vectorCurve * value = vectorCurve * value

instance
  Multiplication
    (Curve1D units)
    (DirectionCurve3D space)
    (VectorCurve3D units space)
  where
  scalarCurve * DirectionCurve3D vectorCurve = scalarCurve * vectorCurve

instance
  Multiplication
    (DirectionCurve3D space)
    (Curve1D units)
    (VectorCurve3D units space)
  where
  DirectionCurve3D vectorCurve * scalarCurve = vectorCurve * scalarCurve

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve3D space1) (DirectionCurve3D space2) (Curve1D Unitless)
  where
  DirectionCurve3D lhs `dot` DirectionCurve3D rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve3D space1) (VectorCurve3D units space2) (Curve1D units)
  where
  DirectionCurve3D lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (VectorCurve3D units space1) (DirectionCurve3D space2) (Curve1D units)
  where
  lhs `dot` DirectionCurve3D rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve3D space1) (Direction3D space2) (Curve1D Unitless)
  where
  DirectionCurve3D lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (Direction3D space1) (DirectionCurve3D space2) (Curve1D Unitless)
  where
  lhs `dot` DirectionCurve3D rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve3D space1) (Vector3D units space2) (Curve1D units)
  where
  DirectionCurve3D lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (Vector3D units space1) (DirectionCurve3D space2) (Curve1D units)
  where
  lhs `dot` DirectionCurve3D rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionCurve3D space1)
    (DirectionCurve3D space2)
    (VectorCurve3D Unitless space1)
  where
  DirectionCurve3D lhs `cross` DirectionCurve3D rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionCurve3D space1)
    (VectorCurve3D units space2)
    (VectorCurve3D units space1)
  where
  DirectionCurve3D lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorCurve3D units space1)
    (DirectionCurve3D space2)
    (VectorCurve3D units space1)
  where
  lhs `cross` DirectionCurve3D rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionCurve3D space1)
    (Direction3D space2)
    (VectorCurve3D Unitless space1)
  where
  DirectionCurve3D lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3D space1)
    (DirectionCurve3D space2)
    (VectorCurve3D Unitless space1)
  where
  lhs `cross` DirectionCurve3D rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionCurve3D space1)
    (Vector3D units space2)
    (VectorCurve3D units space1)
  where
  DirectionCurve3D lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Vector3D units space1)
    (DirectionCurve3D space2)
    (VectorCurve3D units space1)
  where
  lhs `cross` DirectionCurve3D rhs = lhs `cross` rhs

instance Composition (DirectionCurve3D space) (Curve1D Unitless) (DirectionCurve3D space) where
  DirectionCurve3D curve `compose` curve1D = DirectionCurve3D (curve `compose` curve1D)

placeIn ::
  Frame3D global local ->
  DirectionCurve3D local ->
  DirectionCurve3D global
placeIn frame (DirectionCurve3D curve) =
  DirectionCurve3D (VectorCurve3D.placeIn frame curve)

relativeTo ::
  Frame3D global local ->
  DirectionCurve3D global ->
  DirectionCurve3D local
relativeTo frame = placeIn (Frame3D.inverse frame)
