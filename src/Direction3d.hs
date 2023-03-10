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

type Direction3d :: Type -> Type
data Direction3d coordinates = Direction3d# {xComponent :: Float, yComponent :: Float, zComponent :: Float}
  deriving (Eq, Show)

{-# COMPLETE Direction3d #-}

{-# INLINE Direction3d #-}
pattern Direction3d :: Float -> Float -> Float -> Direction3d coordinates
pattern Direction3d x y z <- Direction3d# x y z

instance
  coordinates ~ coordinates'
  => ApproximateEquality (Direction3d coordinates) (Direction3d coordinates) Radians
  where
  d1 ~= d2 = angleFrom d1 d2 ~= Qty.zero

instance Negation (Direction3d coordinates) where
  negate (Direction3d dx dy dz) = unsafe (negate dx) (negate dy) (negate dz)

instance coordinates ~ coordinates' => DotProduct (Direction3d coordinates) (Direction3d coordinates') Float where
  Direction3d x1 y1 z1 <> Direction3d x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

instance coordinates ~ coordinates' => DotProduct (Vector3d coordinates units) (Direction3d coordinates') (Qty units) where
  Vector3d vx vy vz <> Direction3d dx dy dz = vx * dx + vy * dy + vz * dz

instance coordinates ~ coordinates' => DotProduct (Direction3d coordinates) (Vector3d coordinates' units) (Qty units) where
  Direction3d dx dy dz <> Vector3d vx vy vz = dx * vx + dy * vy + dz * vz

instance Multiplication (Qty units) (Direction3d coordinates) (Vector3d coordinates units) where
  scale * Direction3d dx dy dz = Vector3d (scale * dx) (scale * dy) (scale * dz)

instance Multiplication (Direction3d coordinates) (Qty units) (Vector3d coordinates units) where
  Direction3d dx dy dz * scale = Vector3d (dx * scale) (dy * scale) (dz * scale)

instance coordinates ~ coordinates' => CrossProduct (Vector3d coordinates units) (Direction3d coordinates') (Vector3d coordinates units) where
  Vector3d x1 y1 z1 >< Direction3d x2 y2 z2 =
    Vector3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

instance coordinates ~ coordinates' => CrossProduct (Direction3d coordinates) (Vector3d coordinates' units) (Vector3d coordinates units) where
  Direction3d x1 y1 z1 >< Vector3d x2 y2 z2 =
    Vector3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

instance coordinates ~ coordinates' => CrossProduct (Direction3d coordinates) (Direction3d coordinates') (Vector3d coordinates Unitless) where
  Direction3d x1 y1 z1 >< Direction3d x2 y2 z2 =
    Vector3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

unsafe :: Float -> Float -> Float -> Direction3d coordinates
unsafe = Direction3d#

positiveX :: Direction3d coordinates
positiveX = unsafe 1.0 0.0 0.0

negativeX :: Direction3d coordinates
negativeX = negate positiveX

positiveY :: Direction3d coordinates
positiveY = unsafe 0.0 1.0 0.0

negativeY :: Direction3d coordinates
negativeY = negate positiveY

positiveZ :: Direction3d coordinates
positiveZ = unsafe 0.0 0.0 1.0

negativeZ :: Direction3d coordinates
negativeZ = negate positiveZ

x :: Direction3d coordinates
x = positiveX

y :: Direction3d coordinates
y = positiveY

z :: Direction3d coordinates
z = positiveZ

angleFrom :: Direction3d coordinates -> Direction3d coordinates -> Angle
angleFrom d1 d2 = Angle.atan2 (Vector3d.magnitude (d1 >< d2)) (d1 <> d2)
