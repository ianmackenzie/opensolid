module DirectionCurve2d
  ( DirectionCurve2d
  , unsafe
  , unwrap
  , evaluateAt
  , segmentBounds
  , derivative
  , constant
  , reverse
  , xComponent
  , yComponent
  )
where

import Curve1d (Curve1d)
import Direction2d (Direction2d (Direction2d))
import Direction2d qualified
import DirectionBox2d (DirectionBox2d)
import DirectionBox2d qualified
import Domain (Domain)
import OpenSolid
import Vector2d (Vector2d)
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

newtype DirectionCurve2d space = DirectionCurve2d (VectorCurve2d (space @ Unitless))
  deriving (Show)

unsafe :: VectorCurve2d (space @ Unitless) -> DirectionCurve2d space
unsafe = DirectionCurve2d

unwrap :: DirectionCurve2d space -> VectorCurve2d (space @ Unitless)
unwrap (DirectionCurve2d vectorCurve) = vectorCurve

evaluateAt :: Float -> DirectionCurve2d space -> Direction2d space
evaluateAt t (DirectionCurve2d vectorCurve) =
  Direction2d.unsafe (VectorCurve2d.evaluateAt t vectorCurve)

segmentBounds :: Domain -> DirectionCurve2d space -> DirectionBox2d space
segmentBounds t (DirectionCurve2d vectorCurve) =
  DirectionBox2d.unsafe (VectorCurve2d.segmentBounds t vectorCurve)

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

instance
  space ~ space' =>
  DotProduct
    (DirectionCurve2d space)
    (DirectionCurve2d space')
    (Curve1d Unitless)
  where
  DirectionCurve2d curve1 <> DirectionCurve2d curve2 = curve1 <> curve2

instance
  space ~ space' =>
  DotProduct
    (DirectionCurve2d space)
    (VectorCurve2d (space' @ units))
    (Curve1d units)
  where
  DirectionCurve2d curve1 <> curve2 = curve1 <> curve2

instance
  space ~ space' =>
  DotProduct
    (VectorCurve2d (space @ units))
    (DirectionCurve2d space')
    (Curve1d units)
  where
  curve1 <> DirectionCurve2d curve2 = curve1 <> curve2

instance
  space ~ space' =>
  DotProduct
    (DirectionCurve2d space)
    (Direction2d space')
    (Curve1d Unitless)
  where
  DirectionCurve2d curve <> direction = curve <> direction

instance
  space ~ space' =>
  DotProduct
    (Direction2d space)
    (DirectionCurve2d space')
    (Curve1d Unitless)
  where
  direction <> DirectionCurve2d curve = direction <> curve

instance
  space ~ space' =>
  DotProduct
    (DirectionCurve2d space)
    (Vector2d (space' @ units))
    (Curve1d units)
  where
  DirectionCurve2d curve <> vector = curve <> vector

instance
  space ~ space' =>
  DotProduct
    (Vector2d (space @ units))
    (DirectionCurve2d space')
    (Curve1d units)
  where
  vector <> DirectionCurve2d curve = vector <> curve

instance
  space ~ space' =>
  CrossProduct
    (DirectionCurve2d space)
    (DirectionCurve2d space')
    (Curve1d Unitless)
  where
  DirectionCurve2d curve1 >< DirectionCurve2d curve2 = curve1 >< curve2

instance
  space ~ space' =>
  CrossProduct
    (DirectionCurve2d space)
    (VectorCurve2d (space' @ units))
    (Curve1d units)
  where
  DirectionCurve2d curve1 >< curve2 = curve1 >< curve2

instance
  space ~ space' =>
  CrossProduct
    (VectorCurve2d (space @ units))
    (DirectionCurve2d space')
    (Curve1d units)
  where
  curve1 >< DirectionCurve2d curve2 = curve1 >< curve2

instance
  space ~ space' =>
  CrossProduct
    (DirectionCurve2d space)
    (Direction2d space')
    (Curve1d Unitless)
  where
  DirectionCurve2d curve >< direction = curve >< direction

instance
  space ~ space' =>
  CrossProduct
    (Direction2d space)
    (DirectionCurve2d space')
    (Curve1d Unitless)
  where
  direction >< DirectionCurve2d curve = direction >< curve

instance
  space ~ space' =>
  CrossProduct
    (DirectionCurve2d space)
    (Vector2d (space' @ units))
    (Curve1d units)
  where
  DirectionCurve2d curve >< vector = curve >< vector

instance
  space ~ space' =>
  CrossProduct
    (Vector2d (space @ units))
    (DirectionCurve2d space')
    (Curve1d units)
  where
  vector >< DirectionCurve2d curve = vector >< curve

xComponent :: DirectionCurve2d space -> Curve1d Unitless
xComponent curve = curve <> Direction2d.x

yComponent :: DirectionCurve2d space -> Curve1d Unitless
yComponent curve = curve <> Direction2d.y
