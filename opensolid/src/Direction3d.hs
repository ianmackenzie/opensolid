module Direction3d
  ( Direction3d (Direction3d)
  , xComponent
  , yComponent
  , zComponent
  , unsafe
  , unwrap
  , x
  , y
  , z
  , positiveX
  , negativeX
  , positiveY
  , negativeY
  , positiveZ
  , negativeZ
  , angleFrom
  )
where

import Angle qualified
import OpenSolid
import Units qualified
import Vector3d (Vector3d (Vector3d))
import Vector3d qualified

type role Direction3d phantom

newtype Direction3d (space :: Type) = Direction3d_ (Vector3d (space @ Unitless))
  deriving (Eq, Show)

{-# COMPLETE Direction3d #-}

{-# INLINE Direction3d #-}
pattern Direction3d :: Vector3d (space @ Unitless) -> Direction3d space
pattern Direction3d v <- Direction3d_ v

instance HasUnits (Direction3d space) where
  type Units (Direction3d space) = Unitless
  type Erase (Direction3d space) = Direction3d space

instance space ~ space' => Units.Coercion (Direction3d space) (Direction3d space') where
  coerce = identity

instance
  space ~ space' =>
  ApproximateEquality (Direction3d space) (Direction3d space') Radians
  where
  d1 ~= d2 = angleFrom d1 d2 ~= Angle.zero

instance Negation (Direction3d space) where
  negate (Direction3d vector) = unsafe (negate vector)

instance Product Sign (Direction3d space) (Direction3d space)

instance Multiplication Sign (Direction3d space) where
  type Sign .*. Direction3d space = Direction3d space
  Positive .*. direction = direction
  Negative .*. direction = -direction

instance Product (Direction3d space) Sign (Direction3d space)

instance Multiplication (Direction3d space) Sign where
  type Direction3d space .*. Sign = Direction3d space
  direction .*. Positive = direction
  direction .*. Negative = -direction

instance space ~ space' => DotProduct (Direction3d space) (Direction3d space') Float

instance space ~ space' => DotMultiplication (Direction3d space) (Direction3d space') where
  type Direction3d space .<>. Direction3d space' = Qty (Unitless :*: Unitless)
  Direction3d vector1 .<>. Direction3d vector2 = vector1 .<>. vector2

instance space ~ space' => DotProduct (Vector3d (space @ units)) (Direction3d space') (Qty units)

instance space ~ space' => DotMultiplication (Vector3d (space @ units)) (Direction3d space') where
  type Vector3d (space @ units) .<>. Direction3d space' = Qty (units :*: Unitless)
  vector1 .<>. Direction3d vector2 = vector1 .<>. vector2

instance space ~ space' => DotProduct (Direction3d space) (Vector3d (space' @ units)) (Qty units)

instance space ~ space' => DotMultiplication (Direction3d space) (Vector3d (space' @ units)) where
  type Direction3d space .<>. Vector3d (space' @ units) = Qty (Unitless :*: units)
  Direction3d vector1 .<>. vector2 = vector1 .<>. vector2

instance Product (Qty units) (Direction3d space) (Vector3d (space @ units))

instance Multiplication (Qty units) (Direction3d space) where
  type Qty units .*. Direction3d space = Vector3d (space @ (units :*: Unitless))
  scale .*. Direction3d vector = scale .*. vector

instance Product (Direction3d space) (Qty units) (Vector3d (space @ units))

instance Multiplication (Direction3d space) (Qty units) where
  type Direction3d space .*. Qty units = Vector3d (space @ (Unitless :*: units))
  Direction3d vector .*. scale = vector .*. scale

instance
  space ~ space' =>
  CrossProduct (Vector3d (space @ units)) (Direction3d space') (Vector3d (space @ units))

instance space ~ space' => CrossMultiplication (Vector3d (space @ units)) (Direction3d space') where
  type Vector3d (space @ units) .><. Direction3d space' = Vector3d (space @ (units :*: Unitless))
  vector1 .><. Direction3d vector2 = vector1 .><. vector2

instance
  space ~ space' =>
  CrossProduct (Direction3d space) (Vector3d (space' @ units)) (Vector3d (space @ units))

instance space ~ space' => CrossMultiplication (Direction3d space) (Vector3d (space' @ units)) where
  type Direction3d space .><. Vector3d (space' @ units) = Vector3d (space @ (Unitless :*: units))
  Direction3d vector1 .><. vector2 = vector1 .><. vector2

instance
  space ~ space' =>
  CrossProduct (Direction3d space) (Direction3d space') (Vector3d (space @ Unitless))

instance space ~ space' => CrossMultiplication (Direction3d space) (Direction3d space') where
  type Direction3d space .><. Direction3d space' = Vector3d (space @ (Unitless :*: Unitless))
  Direction3d vector1 .><. Direction3d vector2 = vector1 .><. vector2

xComponent :: Direction3d space -> Float
xComponent (Direction3d_ vector) = Vector3d.xComponent vector

yComponent :: Direction3d space -> Float
yComponent (Direction3d_ vector) = Vector3d.yComponent vector

zComponent :: Direction3d space -> Float
zComponent (Direction3d_ vector) = Vector3d.zComponent vector

unsafe :: Vector3d (space @ Unitless) -> Direction3d space
unsafe = Direction3d_

unwrap :: Direction3d space -> Vector3d (space @ Unitless)
unwrap (Direction3d vector) = vector

positiveX :: Direction3d space
positiveX = unsafe (Vector3d 1.0 0.0 0.0)

negativeX :: Direction3d space
negativeX = negate positiveX

positiveY :: Direction3d space
positiveY = unsafe (Vector3d 0.0 1.0 0.0)

negativeY :: Direction3d space
negativeY = negate positiveY

positiveZ :: Direction3d space
positiveZ = unsafe (Vector3d 0.0 0.0 1.0)

negativeZ :: Direction3d space
negativeZ = negate positiveZ

x :: Direction3d space
x = positiveX

y :: Direction3d space
y = positiveY

z :: Direction3d space
z = positiveZ

angleFrom :: Direction3d space -> Direction3d space -> Angle
angleFrom d1 d2 = Angle.atan2 (Vector3d.magnitude (d1 >< d2)) (d1 <> d2)
