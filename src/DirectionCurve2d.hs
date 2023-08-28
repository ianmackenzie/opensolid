module DirectionCurve2d
  ( DirectionCurve2d
  , evaluateAt
  , derivative
  , constant
  , reverse
  )
where

import Curve1d (Curve1d)
import Direction2d (Direction2d (Direction2d))
import Direction2d qualified
import OpenSolid
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

newtype DirectionCurve2d space = DirectionCurve2d (VectorCurve2d (space @ Unitless))

evaluateAt :: Float -> DirectionCurve2d space -> Direction2d space
evaluateAt t (DirectionCurve2d vectorCurve) =
  Direction2d.unsafe (VectorCurve2d.evaluateAt t vectorCurve)

derivative :: DirectionCurve2d space -> VectorCurve2d (space @ Unitless)
derivative (DirectionCurve2d vectorCurve) = VectorCurve2d.derivative vectorCurve

constant :: Direction2d space -> DirectionCurve2d space
constant (Direction2d vector) = DirectionCurve2d (VectorCurve2d.constant vector)

reverse :: DirectionCurve2d space -> DirectionCurve2d space
reverse (DirectionCurve2d vectorCurve) = DirectionCurve2d (VectorCurve2d.reverse vectorCurve)

instance Negation (DirectionCurve2d space) where
  negate (DirectionCurve2d vectorCurve) = DirectionCurve2d (negate vectorCurve)

instance
  Multiplication
    Sign
    (DirectionCurve2d space)
    (DirectionCurve2d space)
  where
  Positive * curve = curve
  Negative * curve = -curve

instance
  Multiplication
    (DirectionCurve2d space)
    Sign
    (DirectionCurve2d space)
  where
  curve * Positive = curve
  curve * Negative = -curve

instance
  Multiplication
    (Qty units)
    (DirectionCurve2d space)
    (VectorCurve2d (space @ units))
  where
  value * DirectionCurve2d vectorCurve = value * vectorCurve

instance
  Multiplication
    (DirectionCurve2d space)
    (Qty units)
    (VectorCurve2d (space @ units))
  where
  DirectionCurve2d vectorCurve * value = vectorCurve * value

instance
  Multiplication
    (Curve1d units)
    (DirectionCurve2d space)
    (VectorCurve2d (space @ units))
  where
  scalarCurve * DirectionCurve2d vectorCurve = scalarCurve * vectorCurve

instance
  Multiplication
    (DirectionCurve2d space)
    (Curve1d units)
    (VectorCurve2d (space @ units))
  where
  DirectionCurve2d vectorCurve * scalarCurve = vectorCurve * scalarCurve
