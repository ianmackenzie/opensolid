module DirectionBounds2d
  ( DirectionBounds2d
  , unsafe
  , unwrap
  , constant
  , hull2
  , hull3
  , hull4
  , xComponent
  , yComponent
  )
where

import Direction2d (Direction2d (Direction2d))
import OpenSolid
import Range (Range)
import Vector2d (Vector2d)
import VectorBounds2d (VectorBounds2d)
import VectorBounds2d qualified

newtype DirectionBounds2d space
  = DirectionBounds2d (VectorBounds2d (space @ Unitless))
  deriving (Show)

instance Negation (DirectionBounds2d space) where
  negate (DirectionBounds2d vectorBounds) = DirectionBounds2d (negate vectorBounds)

instance Multiplication Sign (DirectionBounds2d space) (DirectionBounds2d space) where
  Positive * directionBounds = directionBounds
  Negative * directionBounds = -directionBounds

instance Multiplication (DirectionBounds2d space) Sign (DirectionBounds2d space) where
  directionBounds * Positive = directionBounds
  directionBounds * Negative = -directionBounds

instance
  Multiplication
    (Qty units)
    (DirectionBounds2d space)
    (VectorBounds2d (space @ units))
  where
  value * DirectionBounds2d vectorBounds = value * vectorBounds

instance
  Multiplication
    (DirectionBounds2d space)
    (Qty units)
    (VectorBounds2d (space @ units))
  where
  DirectionBounds2d vectorBounds * value = vectorBounds * value

instance
  Multiplication
    (Range units)
    (DirectionBounds2d space)
    (VectorBounds2d (space @ units))
  where
  range * DirectionBounds2d vectorBounds = range * vectorBounds

instance
  Multiplication
    (DirectionBounds2d space)
    (Range units)
    (VectorBounds2d (space @ units))
  where
  DirectionBounds2d vectorBounds * range = vectorBounds * range

instance
  (space ~ space') =>
  DotProduct
    (DirectionBounds2d space)
    (DirectionBounds2d space')
    (Range Unitless)
  where
  DirectionBounds2d vectorBounds1 <> DirectionBounds2d vectorBounds2 = vectorBounds1 <> vectorBounds2

instance
  (space ~ space') =>
  DotProduct
    (DirectionBounds2d space)
    (VectorBounds2d (space @ units))
    (Range units)
  where
  DirectionBounds2d vectorBounds1 <> vectorBounds2 = vectorBounds1 <> vectorBounds2

instance
  (space ~ space') =>
  DotProduct
    (VectorBounds2d (space @ units))
    (DirectionBounds2d space)
    (Range units)
  where
  vectorBounds1 <> DirectionBounds2d vectorBounds2 = vectorBounds1 <> vectorBounds2

instance
  (space ~ space') =>
  DotProduct
    (DirectionBounds2d space)
    (Direction2d space')
    (Range Unitless)
  where
  DirectionBounds2d vectorBounds <> direction = vectorBounds <> direction

instance
  (space ~ space') =>
  DotProduct
    (Direction2d space)
    (DirectionBounds2d space')
    (Range Unitless)
  where
  direction <> DirectionBounds2d vectorBounds = direction <> vectorBounds

instance
  (space ~ space') =>
  DotProduct
    (DirectionBounds2d space)
    (Vector2d (space @ units))
    (Range units)
  where
  DirectionBounds2d vectorBounds <> vector = vectorBounds <> vector

instance
  (space ~ space') =>
  DotProduct
    (Vector2d (space @ units))
    (DirectionBounds2d space)
    (Range units)
  where
  vector <> DirectionBounds2d vectorBounds = vector <> vectorBounds

instance
  (space ~ space') =>
  CrossProduct
    (DirectionBounds2d space)
    (DirectionBounds2d space')
    (Range Unitless)
  where
  DirectionBounds2d vectorBounds1 >< DirectionBounds2d vectorBounds2 = vectorBounds1 >< vectorBounds2

instance
  (space ~ space') =>
  CrossProduct
    (DirectionBounds2d space)
    (VectorBounds2d (space @ units))
    (Range units)
  where
  DirectionBounds2d vectorBounds1 >< vectorBounds2 = vectorBounds1 >< vectorBounds2

instance
  (space ~ space') =>
  CrossProduct
    (VectorBounds2d (space @ units))
    (DirectionBounds2d space)
    (Range units)
  where
  vectorBounds1 >< DirectionBounds2d vectorBounds2 = vectorBounds1 >< vectorBounds2

instance
  (space ~ space') =>
  CrossProduct
    (DirectionBounds2d space)
    (Direction2d space')
    (Range Unitless)
  where
  DirectionBounds2d vectorBounds >< direction = vectorBounds >< direction

instance
  (space ~ space') =>
  CrossProduct
    (Direction2d space)
    (DirectionBounds2d space')
    (Range Unitless)
  where
  direction >< DirectionBounds2d vectorBounds = direction >< vectorBounds

instance
  (space ~ space') =>
  CrossProduct
    (DirectionBounds2d space)
    (Vector2d (space @ units))
    (Range units)
  where
  DirectionBounds2d vectorBounds >< vector = vectorBounds >< vector

instance
  (space ~ space') =>
  CrossProduct
    (Vector2d (space @ units))
    (DirectionBounds2d space)
    (Range units)
  where
  vector >< DirectionBounds2d vectorBounds = vector >< vectorBounds

unsafe :: VectorBounds2d (space @ Unitless) -> DirectionBounds2d space
unsafe = DirectionBounds2d

unwrap :: DirectionBounds2d space -> VectorBounds2d (space @ Unitless)
unwrap (DirectionBounds2d vectorBounds) = vectorBounds

constant :: Direction2d space -> DirectionBounds2d space
constant (Direction2d vector) = DirectionBounds2d (VectorBounds2d.constant vector)

hull2 ::
  Direction2d space ->
  Direction2d space ->
  DirectionBounds2d space
hull2 (Direction2d vector1) (Direction2d vector2) =
  DirectionBounds2d (VectorBounds2d.hull2 vector1 vector2)

hull3 ::
  Direction2d space ->
  Direction2d space ->
  Direction2d space ->
  DirectionBounds2d space
hull3 (Direction2d vector1) (Direction2d vector2) (Direction2d vector3) =
  DirectionBounds2d (VectorBounds2d.hull3 vector1 vector2 vector3)

hull4 ::
  Direction2d space ->
  Direction2d space ->
  Direction2d space ->
  Direction2d space ->
  DirectionBounds2d space
hull4 (Direction2d vector1) (Direction2d vector2) (Direction2d vector3) (Direction2d vector4) =
  DirectionBounds2d (VectorBounds2d.hull4 vector1 vector2 vector3 vector4)

xComponent :: DirectionBounds2d space -> Range Unitless
xComponent (DirectionBounds2d vectorBounds) = VectorBounds2d.xComponent vectorBounds

yComponent :: DirectionBounds2d space -> Range Unitless
yComponent (DirectionBounds2d vectorBounds) = VectorBounds2d.yComponent vectorBounds
