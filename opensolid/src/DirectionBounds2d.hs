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
import Units qualified
import Vector2d (Vector2d)
import VectorBounds2d (VectorBounds2d)
import VectorBounds2d qualified

newtype DirectionBounds2d space
  = DirectionBounds2d (VectorBounds2d (space @ Unitless))
  deriving (Show)

instance HasUnits (DirectionBounds2d space) where
  type Units (DirectionBounds2d space) = Unitless
  type Erase (DirectionBounds2d space) = DirectionBounds2d space

instance space ~ space' => Units.Coercion (DirectionBounds2d space) (DirectionBounds2d space') where
  coerce = identity

instance Negation (DirectionBounds2d space) where
  negate (DirectionBounds2d vectorBounds) = DirectionBounds2d (negate vectorBounds)

instance Product Sign (DirectionBounds2d space) (DirectionBounds2d space)

instance Multiplication Sign (DirectionBounds2d space) where
  type Sign .*. DirectionBounds2d space = DirectionBounds2d space
  Positive .*. directionBounds = directionBounds
  Negative .*. directionBounds = -directionBounds

instance Product (DirectionBounds2d space) Sign (DirectionBounds2d space)

instance Multiplication (DirectionBounds2d space) Sign where
  type DirectionBounds2d space .*. Sign = DirectionBounds2d space
  directionBounds .*. Positive = directionBounds
  directionBounds .*. Negative = -directionBounds

instance Product (Qty units) (DirectionBounds2d space) (VectorBounds2d (space @ units))

instance Multiplication (Qty units) (DirectionBounds2d space) where
  type Qty units .*. DirectionBounds2d space = VectorBounds2d (space @ (units :*: Unitless))
  value .*. DirectionBounds2d vectorBounds = value .*. vectorBounds

instance Product (DirectionBounds2d space) (Qty units) (VectorBounds2d (space @ units))

instance Multiplication (DirectionBounds2d space) (Qty units) where
  type DirectionBounds2d space .*. Qty units = VectorBounds2d (space @ (Unitless :*: units))
  DirectionBounds2d vectorBounds .*. value = vectorBounds .*. value

instance Product (Range units) (DirectionBounds2d space) (VectorBounds2d (space @ units))

instance Multiplication (Range units) (DirectionBounds2d space) where
  type Range units .*. DirectionBounds2d space = VectorBounds2d (space @ (units :*: Unitless))
  range .*. DirectionBounds2d vectorBounds = range .*. vectorBounds

instance Product (DirectionBounds2d space) (Range units) (VectorBounds2d (space @ units))

instance Multiplication (DirectionBounds2d space) (Range units) where
  type DirectionBounds2d space .*. Range units = VectorBounds2d (space @ (Unitless :*: units))
  DirectionBounds2d vectorBounds .*. range = vectorBounds .*. range

instance
  space ~ space' =>
  DotProduct (DirectionBounds2d space) (DirectionBounds2d space') (Range Unitless)

instance
  space ~ space' =>
  DotMultiplication (DirectionBounds2d space) (DirectionBounds2d space')
  where
  type DirectionBounds2d space .<>. DirectionBounds2d space' = Range (Unitless :*: Unitless)
  DirectionBounds2d vectorBounds1 .<>. DirectionBounds2d vectorBounds2 = vectorBounds1 .<>. vectorBounds2

instance
  space ~ space' =>
  DotProduct (DirectionBounds2d space) (VectorBounds2d (space' @ units)) (Range units)

instance
  space ~ space' =>
  DotMultiplication (DirectionBounds2d space) (VectorBounds2d (space' @ units))
  where
  type DirectionBounds2d space .<>. VectorBounds2d (space' @ units) = Range (Unitless :*: units)
  DirectionBounds2d vectorBounds1 .<>. vectorBounds2 = vectorBounds1 .<>. vectorBounds2

instance
  space ~ space' =>
  DotProduct (VectorBounds2d (space @ units)) (DirectionBounds2d space') (Range units)

instance
  space ~ space' =>
  DotMultiplication (VectorBounds2d (space @ units)) (DirectionBounds2d space')
  where
  type VectorBounds2d (space @ units) .<>. DirectionBounds2d space' = Range (units :*: Unitless)

  vectorBounds1 .<>. DirectionBounds2d vectorBounds2 = vectorBounds1 .<>. vectorBounds2

instance
  space ~ space' =>
  DotProduct (DirectionBounds2d space) (Direction2d space') (Range Unitless)

instance
  space ~ space' =>
  DotMultiplication (DirectionBounds2d space) (Direction2d space')
  where
  type DirectionBounds2d space .<>. Direction2d space' = Range (Unitless :*: Unitless)
  DirectionBounds2d vectorBounds .<>. direction = vectorBounds .<>. direction

instance
  space ~ space' =>
  DotProduct (Direction2d space) (DirectionBounds2d space') (Range Unitless)

instance
  space ~ space' =>
  DotMultiplication (Direction2d space) (DirectionBounds2d space')
  where
  type Direction2d space .<>. DirectionBounds2d space' = Range (Unitless :*: Unitless)
  direction .<>. DirectionBounds2d vectorBounds = direction .<>. vectorBounds

instance
  space ~ space' =>
  DotProduct (DirectionBounds2d space) (Vector2d (space' @ units)) (Range units)

instance
  space ~ space' =>
  DotMultiplication (DirectionBounds2d space) (Vector2d (space' @ units))
  where
  type DirectionBounds2d space .<>. Vector2d (space' @ units) = Range (Unitless :*: units)
  DirectionBounds2d vectorBounds .<>. vector = vectorBounds .<>. vector

instance
  space ~ space' =>
  DotProduct (Vector2d (space @ units)) (DirectionBounds2d space') (Range units)

instance
  space ~ space' =>
  DotMultiplication (Vector2d (space @ units)) (DirectionBounds2d space')
  where
  type Vector2d (space @ units) .<>. DirectionBounds2d space' = Range (units :*: Unitless)
  vector .<>. DirectionBounds2d vectorBounds = vector .<>. vectorBounds

instance
  space ~ space' =>
  CrossProduct (DirectionBounds2d space) (DirectionBounds2d space') (Range Unitless)

instance
  space ~ space' =>
  CrossMultiplication (DirectionBounds2d space) (DirectionBounds2d space')
  where
  type DirectionBounds2d space .><. DirectionBounds2d space' = Range (Unitless :*: Unitless)
  DirectionBounds2d vectorBounds1 .><. DirectionBounds2d vectorBounds2 = vectorBounds1 .><. vectorBounds2

instance
  space ~ space' =>
  CrossProduct (DirectionBounds2d space) (VectorBounds2d (space' @ units)) (Range units)

instance
  space ~ space' =>
  CrossMultiplication (DirectionBounds2d space) (VectorBounds2d (space' @ units))
  where
  type DirectionBounds2d space .><. VectorBounds2d (space' @ units) = Range (Unitless :*: units)

  DirectionBounds2d vectorBounds1 .><. vectorBounds2 = vectorBounds1 .><. vectorBounds2

instance
  space ~ space' =>
  CrossProduct (VectorBounds2d (space @ units)) (DirectionBounds2d space') (Range units)

instance
  space ~ space' =>
  CrossMultiplication (VectorBounds2d (space @ units)) (DirectionBounds2d space')
  where
  type VectorBounds2d (space @ units) .><. DirectionBounds2d space' = Range (units :*: Unitless)

  vectorBounds1 .><. DirectionBounds2d vectorBounds2 = vectorBounds1 .><. vectorBounds2

instance
  space ~ space' =>
  CrossProduct (DirectionBounds2d space) (Direction2d space') (Range Unitless)

instance
  space ~ space' =>
  CrossMultiplication (DirectionBounds2d space) (Direction2d space')
  where
  type DirectionBounds2d space .><. Direction2d space' = Range (Unitless :*: Unitless)
  DirectionBounds2d vectorBounds .><. direction = vectorBounds .><. direction

instance
  space ~ space' =>
  CrossProduct (Direction2d space) (DirectionBounds2d space') (Range Unitless)

instance
  space ~ space' =>
  CrossMultiplication (Direction2d space) (DirectionBounds2d space')
  where
  type Direction2d space .><. DirectionBounds2d space' = Range (Unitless :*: Unitless)
  direction .><. DirectionBounds2d vectorBounds = direction .><. vectorBounds

instance
  space ~ space' =>
  CrossProduct (DirectionBounds2d space) (Vector2d (space' @ units)) (Range units)

instance
  space ~ space' =>
  CrossMultiplication (DirectionBounds2d space) (Vector2d (space' @ units))
  where
  type DirectionBounds2d space .><. Vector2d (space' @ units) = Range (Unitless :*: units)
  DirectionBounds2d vectorBounds .><. vector = vectorBounds .><. vector

instance
  space ~ space' =>
  CrossProduct (Vector2d (space @ units)) (DirectionBounds2d space') (Range units)

instance
  space ~ space' =>
  CrossMultiplication (Vector2d (space @ units)) (DirectionBounds2d space')
  where
  type Vector2d (space @ units) .><. DirectionBounds2d space' = Range (units :*: Unitless)
  vector .><. DirectionBounds2d vectorBounds = vector .><. vectorBounds

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
