module Direction3d
  ( Direction3d (Direction3d, xComponent, yComponent, zComponent)
  , unsafe
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

import Angle (Angle)
import Angle qualified
import OpenSolid
import Qty qualified
import Units (Radians, Unitless)
import Vector3d (Vector3d (..))
import Vector3d qualified

type role Direction3d nominal

data Direction3d (space :: Type) = Direction3d# {xComponent :: Float, yComponent :: Float, zComponent :: Float}
  deriving (Eq, Show)

{-# COMPLETE Direction3d #-}

{-# INLINE Direction3d #-}
pattern Direction3d :: Float -> Float -> Float -> Direction3d space
pattern Direction3d x y z <- Direction3d# x y z

instance
  space ~ space'
  => ApproximateEquality (Direction3d space) (Direction3d space) Radians
  where
  d1 ~= d2 = angleFrom d1 d2 ~= Qty.zero

instance Negation (Direction3d space) where
  negate (Direction3d dx dy dz) = unsafe (negate dx) (negate dy) (negate dz)

instance space ~ space' => DotProduct (Direction3d space) (Direction3d space') Float where
  Direction3d x1 y1 z1 <> Direction3d x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

instance space ~ space' => DotProduct (Vector3d (Coordinates space units)) (Direction3d space') (Qty units) where
  Vector3d vx vy vz <> Direction3d dx dy dz = vx * dx + vy * dy + vz * dz

instance space ~ space' => DotProduct (Direction3d space) (Vector3d (Coordinates space' units)) (Qty units) where
  Direction3d dx dy dz <> Vector3d vx vy vz = dx * vx + dy * vy + dz * vz

instance Multiplication (Qty units) (Direction3d space) (Vector3d (Coordinates space units)) where
  scale * Direction3d dx dy dz = Vector3d (scale * dx) (scale * dy) (scale * dz)

instance Multiplication (Direction3d space) (Qty units) (Vector3d (Coordinates space units)) where
  Direction3d dx dy dz * scale = Vector3d (dx * scale) (dy * scale) (dz * scale)

instance space ~ space' => CrossProduct (Vector3d (Coordinates space units)) (Direction3d space') (Vector3d (Coordinates space units)) where
  Vector3d x1 y1 z1 >< Direction3d x2 y2 z2 =
    Vector3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

instance space ~ space' => CrossProduct (Direction3d space) (Vector3d (Coordinates space' units)) (Vector3d (Coordinates space units)) where
  Direction3d x1 y1 z1 >< Vector3d x2 y2 z2 =
    Vector3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

instance space ~ space' => CrossProduct (Direction3d space) (Direction3d space') (Vector3d (Coordinates space Unitless)) where
  Direction3d x1 y1 z1 >< Direction3d x2 y2 z2 =
    Vector3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

unsafe :: Float -> Float -> Float -> Direction3d space
unsafe = Direction3d#

positiveX :: Direction3d space
positiveX = unsafe 1.0 0.0 0.0

negativeX :: Direction3d space
negativeX = negate positiveX

positiveY :: Direction3d space
positiveY = unsafe 0.0 1.0 0.0

negativeY :: Direction3d space
negativeY = negate positiveY

positiveZ :: Direction3d space
positiveZ = unsafe 0.0 0.0 1.0

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
