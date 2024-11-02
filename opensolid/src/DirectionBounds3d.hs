module DirectionBounds3d
  ( DirectionBounds3d
  , unsafe
  , unwrap
  , constant
  , xComponent
  , yComponent
  , zComponent
  )
where

import Direction3d (Direction3d)
import OpenSolid
import Range (Range)
import Units qualified
import Vector3d (Vector3d)
import Vector3d qualified
import VectorBounds3d (VectorBounds3d)
import VectorBounds3d qualified

newtype DirectionBounds3d space
  = DirectionBounds3d (VectorBounds3d (space @ Unitless))
  deriving (Show)

instance HasUnits (DirectionBounds3d space) where
  type UnitsOf (DirectionBounds3d space) = Unitless

instance
  space1 ~ space2 =>
  Units.Coercion (DirectionBounds3d space1) (DirectionBounds3d space2)
  where
  coerce = identity

instance Negation (DirectionBounds3d space) where
  negate (DirectionBounds3d vectorBounds) = DirectionBounds3d (negate vectorBounds)

instance Multiplication Sign (DirectionBounds3d space) (DirectionBounds3d space)

instance Multiplication' Sign (DirectionBounds3d space) where
  type Sign .*. DirectionBounds3d space = DirectionBounds3d space
  Positive .*. directionBounds = directionBounds
  Negative .*. directionBounds = -directionBounds

instance Multiplication (DirectionBounds3d space) Sign (DirectionBounds3d space)

instance Multiplication' (DirectionBounds3d space) Sign where
  type DirectionBounds3d space .*. Sign = DirectionBounds3d space
  directionBounds .*. Positive = directionBounds
  directionBounds .*. Negative = -directionBounds

instance Multiplication (Qty units) (DirectionBounds3d space) (VectorBounds3d (space @ units))

instance Multiplication' (Qty units) (DirectionBounds3d space) where
  type Qty units .*. DirectionBounds3d space = VectorBounds3d (space @ (units :*: Unitless))
  value .*. DirectionBounds3d vectorBounds = value .*. vectorBounds

instance Multiplication (DirectionBounds3d space) (Qty units) (VectorBounds3d (space @ units))

instance Multiplication' (DirectionBounds3d space) (Qty units) where
  type DirectionBounds3d space .*. Qty units = VectorBounds3d (space @ (Unitless :*: units))
  DirectionBounds3d vectorBounds .*. value = vectorBounds .*. value

instance Multiplication (Range units) (DirectionBounds3d space) (VectorBounds3d (space @ units))

instance Multiplication' (Range units) (DirectionBounds3d space) where
  type Range units .*. DirectionBounds3d space = VectorBounds3d (space @ (units :*: Unitless))
  range .*. DirectionBounds3d vectorBounds = range .*. vectorBounds

instance Multiplication (DirectionBounds3d space) (Range units) (VectorBounds3d (space @ units))

instance Multiplication' (DirectionBounds3d space) (Range units) where
  type DirectionBounds3d space .*. Range units = VectorBounds3d (space @ (Unitless :*: units))
  DirectionBounds3d vectorBounds .*. range = vectorBounds .*. range

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds3d space1) (DirectionBounds3d space2) (Range Unitless)

instance
  space1 ~ space2 =>
  DotMultiplication' (DirectionBounds3d space1) (DirectionBounds3d space2)
  where
  type DirectionBounds3d space1 .<>. DirectionBounds3d space2 = Range (Unitless :*: Unitless)
  DirectionBounds3d vectorBounds1 .<>. DirectionBounds3d vectorBounds2 =
    vectorBounds1 .<>. vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds3d space1) (VectorBounds3d (space2 @ units)) (Range units)

instance
  space1 ~ space2 =>
  DotMultiplication' (DirectionBounds3d space1) (VectorBounds3d (space2 @ units))
  where
  type DirectionBounds3d space1 .<>. VectorBounds3d (space2 @ units) = Range (Unitless :*: units)
  DirectionBounds3d vectorBounds1 .<>. vectorBounds2 = vectorBounds1 .<>. vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (VectorBounds3d (space1 @ units)) (DirectionBounds3d space2) (Range units)

instance
  space1 ~ space2 =>
  DotMultiplication' (VectorBounds3d (space1 @ units)) (DirectionBounds3d space2)
  where
  type VectorBounds3d (space1 @ units) .<>. DirectionBounds3d space2 = Range (units :*: Unitless)
  vectorBounds1 .<>. DirectionBounds3d vectorBounds2 = vectorBounds1 .<>. vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds3d space1) (Direction3d space2) (Range Unitless)

instance
  space1 ~ space2 =>
  DotMultiplication' (DirectionBounds3d space1) (Direction3d space2)
  where
  type DirectionBounds3d space1 .<>. Direction3d space2 = Range (Unitless :*: Unitless)
  DirectionBounds3d vectorBounds .<>. direction = vectorBounds .<>. direction

instance
  space1 ~ space2 =>
  DotMultiplication (Direction3d space1) (DirectionBounds3d space2) (Range Unitless)

instance
  space1 ~ space2 =>
  DotMultiplication' (Direction3d space1) (DirectionBounds3d space2)
  where
  type Direction3d space1 .<>. DirectionBounds3d space2 = Range (Unitless :*: Unitless)
  direction .<>. DirectionBounds3d vectorBounds = direction .<>. vectorBounds

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds3d space1) (Vector3d (space2 @ units)) (Range units)

instance
  space1 ~ space2 =>
  DotMultiplication' (DirectionBounds3d space1) (Vector3d (space2 @ units))
  where
  type DirectionBounds3d space1 .<>. Vector3d (space2 @ units) = Range (Unitless :*: units)
  DirectionBounds3d vectorBounds .<>. vector = vectorBounds .<>. vector

instance
  space1 ~ space2 =>
  DotMultiplication (Vector3d (space1 @ units)) (DirectionBounds3d space2) (Range units)

instance
  space1 ~ space2 =>
  DotMultiplication' (Vector3d (space1 @ units)) (DirectionBounds3d space2)
  where
  type Vector3d (space1 @ units) .<>. DirectionBounds3d space2 = Range (units :*: Unitless)
  vector .<>. DirectionBounds3d vectorBounds = vector .<>. vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionBounds3d space1)
    (DirectionBounds3d space2)
    (VectorBounds3d (space1 @ Unitless))

instance
  space1 ~ space2 =>
  CrossMultiplication' (DirectionBounds3d space1) (DirectionBounds3d space2)
  where
  type
    DirectionBounds3d space1 .><. DirectionBounds3d space2 =
      VectorBounds3d (space1 @ (Unitless :*: Unitless))
  DirectionBounds3d vectorBounds1 .><. DirectionBounds3d vectorBounds2 =
    vectorBounds1 .><. vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionBounds3d space1)
    (VectorBounds3d (space2 @ units))
    (VectorBounds3d (space1 @ units))

instance
  space1 ~ space2 =>
  CrossMultiplication' (DirectionBounds3d space1) (VectorBounds3d (space2 @ units))
  where
  type
    DirectionBounds3d space1 .><. VectorBounds3d (space2 @ units) =
      VectorBounds3d (space1 @ (Unitless :*: units))
  DirectionBounds3d vectorBounds1 .><. vectorBounds2 = vectorBounds1 .><. vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorBounds3d (space1 @ units))
    (DirectionBounds3d space2)
    (VectorBounds3d (space1 @ units))

instance
  space1 ~ space2 =>
  CrossMultiplication' (VectorBounds3d (space1 @ units)) (DirectionBounds3d space2)
  where
  type
    VectorBounds3d (space1 @ units) .><. DirectionBounds3d space2 =
      VectorBounds3d (space1 @ (units :*: Unitless))
  vectorBounds1 .><. DirectionBounds3d vectorBounds2 = vectorBounds1 .><. vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionBounds3d space1)
    (Direction3d space2)
    (VectorBounds3d (space1 @ Unitless))

instance
  space1 ~ space2 =>
  CrossMultiplication' (DirectionBounds3d space1) (Direction3d space2)
  where
  type
    DirectionBounds3d space1 .><. Direction3d space2 =
      VectorBounds3d (space1 @ (Unitless :*: Unitless))
  DirectionBounds3d vectorBounds .><. direction = vectorBounds .><. direction

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3d space1)
    (DirectionBounds3d space2)
    (VectorBounds3d (space1 @ Unitless))

instance
  space1 ~ space2 =>
  CrossMultiplication' (Direction3d space1) (DirectionBounds3d space2)
  where
  type
    Direction3d space1 .><. DirectionBounds3d space2 =
      VectorBounds3d (space1 @ (Unitless :*: Unitless))
  direction .><. DirectionBounds3d vectorBounds = direction .><. vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionBounds3d space1)
    (Vector3d (space2 @ units))
    (VectorBounds3d (space1 @ units))

instance
  space1 ~ space2 =>
  CrossMultiplication' (DirectionBounds3d space1) (Vector3d (space2 @ units))
  where
  type
    DirectionBounds3d space1 .><. Vector3d (space2 @ units) =
      VectorBounds3d (space1 @ (Unitless :*: units))
  DirectionBounds3d vectorBounds .><. vector = vectorBounds .><. vector

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Vector3d (space1 @ units))
    (DirectionBounds3d space2)
    (VectorBounds3d (space1 @ units))

instance
  space1 ~ space2 =>
  CrossMultiplication' (Vector3d (space1 @ units)) (DirectionBounds3d space2)
  where
  type
    Vector3d (space1 @ units) .><. DirectionBounds3d space2 =
      VectorBounds3d (space1 @ (units :*: Unitless))
  vector .><. DirectionBounds3d vectorBounds = vector .><. vectorBounds

unsafe :: VectorBounds3d (space @ Unitless) -> DirectionBounds3d space
unsafe = DirectionBounds3d

unwrap :: DirectionBounds3d space -> VectorBounds3d (space @ Unitless)
unwrap (DirectionBounds3d vectorBounds) = vectorBounds

constant :: Direction3d space -> DirectionBounds3d space
constant direction = DirectionBounds3d (VectorBounds3d.constant (Vector3d.unit direction))

xComponent :: DirectionBounds3d space -> Range Unitless
xComponent (DirectionBounds3d vectorBounds) = VectorBounds3d.xComponent vectorBounds

yComponent :: DirectionBounds3d space -> Range Unitless
yComponent (DirectionBounds3d vectorBounds) = VectorBounds3d.yComponent vectorBounds

zComponent :: DirectionBounds3d space -> Range Unitless
zComponent (DirectionBounds3d vectorBounds) = VectorBounds3d.zComponent vectorBounds
