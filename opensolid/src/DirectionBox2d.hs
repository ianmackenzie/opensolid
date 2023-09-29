module DirectionBox2d
  ( DirectionBox2d
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
import VectorBox2d (VectorBox2d)
import VectorBox2d qualified

newtype DirectionBox2d space
  = DirectionBox2d (VectorBox2d (space @ Unitless))
  deriving (Show)

instance Negation (DirectionBox2d space) where
  negate (DirectionBox2d vectorBox) = DirectionBox2d (negate vectorBox)

instance Multiplication Sign (DirectionBox2d space) (DirectionBox2d space) where
  Positive * directionBox = directionBox
  Negative * directionBox = -directionBox

instance Multiplication (DirectionBox2d space) Sign (DirectionBox2d space) where
  directionBox * Positive = directionBox
  directionBox * Negative = -directionBox

instance
  Multiplication
    (Qty units)
    (DirectionBox2d space)
    (VectorBox2d (space @ units))
  where
  value * DirectionBox2d vectorBox = value * vectorBox

instance
  Multiplication
    (DirectionBox2d space)
    (Qty units)
    (VectorBox2d (space @ units))
  where
  DirectionBox2d vectorBox * value = vectorBox * value

instance
  Multiplication
    (Range units)
    (DirectionBox2d space)
    (VectorBox2d (space @ units))
  where
  range * DirectionBox2d vectorBox = range * vectorBox

instance
  Multiplication
    (DirectionBox2d space)
    (Range units)
    (VectorBox2d (space @ units))
  where
  DirectionBox2d vectorBox * range = vectorBox * range

instance
  (space ~ space') =>
  DotProduct
    (DirectionBox2d space)
    (DirectionBox2d space')
    (Range Unitless)
  where
  DirectionBox2d vectorBox1 <> DirectionBox2d vectorBox2 = vectorBox1 <> vectorBox2

instance
  (space ~ space') =>
  DotProduct
    (DirectionBox2d space)
    (VectorBox2d (space @ units))
    (Range units)
  where
  DirectionBox2d vectorBox1 <> vectorBox2 = vectorBox1 <> vectorBox2

instance
  (space ~ space') =>
  DotProduct
    (VectorBox2d (space @ units))
    (DirectionBox2d space)
    (Range units)
  where
  vectorBox1 <> DirectionBox2d vectorBox2 = vectorBox1 <> vectorBox2

instance
  (space ~ space') =>
  DotProduct
    (DirectionBox2d space)
    (Direction2d space')
    (Range Unitless)
  where
  DirectionBox2d vectorBox <> direction = vectorBox <> direction

instance
  (space ~ space') =>
  DotProduct
    (Direction2d space)
    (DirectionBox2d space')
    (Range Unitless)
  where
  direction <> DirectionBox2d vectorBox = direction <> vectorBox

instance
  (space ~ space') =>
  DotProduct
    (DirectionBox2d space)
    (Vector2d (space @ units))
    (Range units)
  where
  DirectionBox2d vectorBox <> vector = vectorBox <> vector

instance
  (space ~ space') =>
  DotProduct
    (Vector2d (space @ units))
    (DirectionBox2d space)
    (Range units)
  where
  vector <> DirectionBox2d vectorBox = vector <> vectorBox

instance
  (space ~ space') =>
  CrossProduct
    (DirectionBox2d space)
    (DirectionBox2d space')
    (Range Unitless)
  where
  DirectionBox2d vectorBox1 >< DirectionBox2d vectorBox2 = vectorBox1 >< vectorBox2

instance
  (space ~ space') =>
  CrossProduct
    (DirectionBox2d space)
    (VectorBox2d (space @ units))
    (Range units)
  where
  DirectionBox2d vectorBox1 >< vectorBox2 = vectorBox1 >< vectorBox2

instance
  (space ~ space') =>
  CrossProduct
    (VectorBox2d (space @ units))
    (DirectionBox2d space)
    (Range units)
  where
  vectorBox1 >< DirectionBox2d vectorBox2 = vectorBox1 >< vectorBox2

instance
  (space ~ space') =>
  CrossProduct
    (DirectionBox2d space)
    (Direction2d space')
    (Range Unitless)
  where
  DirectionBox2d vectorBox >< direction = vectorBox >< direction

instance
  (space ~ space') =>
  CrossProduct
    (Direction2d space)
    (DirectionBox2d space')
    (Range Unitless)
  where
  direction >< DirectionBox2d vectorBox = direction >< vectorBox

instance
  (space ~ space') =>
  CrossProduct
    (DirectionBox2d space)
    (Vector2d (space @ units))
    (Range units)
  where
  DirectionBox2d vectorBox >< vector = vectorBox >< vector

instance
  (space ~ space') =>
  CrossProduct
    (Vector2d (space @ units))
    (DirectionBox2d space)
    (Range units)
  where
  vector >< DirectionBox2d vectorBox = vector >< vectorBox

unsafe :: VectorBox2d (space @ Unitless) -> DirectionBox2d space
unsafe = DirectionBox2d

unwrap :: DirectionBox2d space -> VectorBox2d (space @ Unitless)
unwrap (DirectionBox2d vectorBox) = vectorBox

constant :: Direction2d space -> DirectionBox2d space
constant (Direction2d vector) = DirectionBox2d (VectorBox2d.constant vector)

hull2 ::
  Direction2d space ->
  Direction2d space ->
  DirectionBox2d space
hull2 (Direction2d vector1) (Direction2d vector2) =
  DirectionBox2d (VectorBox2d.hull2 vector1 vector2)

hull3 ::
  Direction2d space ->
  Direction2d space ->
  Direction2d space ->
  DirectionBox2d space
hull3 (Direction2d vector1) (Direction2d vector2) (Direction2d vector3) =
  DirectionBox2d (VectorBox2d.hull3 vector1 vector2 vector3)

hull4 ::
  Direction2d space ->
  Direction2d space ->
  Direction2d space ->
  Direction2d space ->
  DirectionBox2d space
hull4 (Direction2d vector1) (Direction2d vector2) (Direction2d vector3) (Direction2d vector4) =
  DirectionBox2d (VectorBox2d.hull4 vector1 vector2 vector3 vector4)

xComponent :: DirectionBox2d space -> Range Unitless
xComponent (DirectionBox2d vectorBox) = VectorBox2d.xComponent vectorBox

yComponent :: DirectionBox2d space -> Range Unitless
yComponent (DirectionBox2d vectorBox) = VectorBox2d.yComponent vectorBox
