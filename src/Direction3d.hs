module Direction3d
  ( Direction3d (Direction3d)
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
import Qty qualified
import Vector3d (Vector3d (Vector3d))
import Vector3d qualified

type role Direction3d phantom

newtype Direction3d (space :: Type) = Direction3d_ (Vector3d (space @ Unitless))
  deriving (Eq, Show)

{-# COMPLETE Direction3d #-}

{-# INLINE Direction3d #-}
pattern Direction3d :: Vector3d (space @ Unitless) -> Direction3d space
pattern Direction3d v <- Direction3d_ v

instance
  space ~ space' =>
  ApproximateEquality (Direction3d space) (Direction3d space') Radians
  where
  d1 ~= d2 = angleFrom d1 d2 ~= Qty.zero

instance Negation (Direction3d space) where
  negate (Direction3d vector) = unsafe (negate vector)

instance Multiplication Sign (Direction3d space) (Direction3d space) where
  Positive * direction = direction
  Negative * direction = -direction

instance Multiplication (Direction3d space) Sign (Direction3d space) where
  direction * Positive = direction
  direction * Negative = -direction

instance space ~ space' => DotProduct (Direction3d space) (Direction3d space') Float where
  Direction3d vector1 <> Direction3d vector2 = vector1 <> vector2

instance space ~ space' => DotProduct (Vector3d (space @ units)) (Direction3d space') (Qty units) where
  vector1 <> Direction3d vector2 = vector1 <> vector2

instance space ~ space' => DotProduct (Direction3d space) (Vector3d (space' @ units)) (Qty units) where
  Direction3d vector1 <> vector2 = vector1 <> vector2

instance Multiplication (Qty units) (Direction3d space) (Vector3d (space @ units)) where
  scale * Direction3d vector = scale * vector

instance Multiplication (Direction3d space) (Qty units) (Vector3d (space @ units)) where
  Direction3d vector * scale = vector * scale

instance space ~ space' => CrossProduct (Vector3d (space @ units)) (Direction3d space') (Vector3d (space @ units)) where
  vector1 >< Direction3d vector2 = vector1 >< vector2

instance space ~ space' => CrossProduct (Direction3d space) (Vector3d (space' @ units)) (Vector3d (space @ units)) where
  Direction3d vector1 >< vector2 = vector1 >< vector2

instance space ~ space' => CrossProduct (Direction3d space) (Direction3d space') (Vector3d (space @ Unitless)) where
  Direction3d vector1 >< Direction3d vector2 = vector1 >< vector2

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
