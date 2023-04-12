module VectorBox3d
  ( VectorBox3d (..)
  , constant
  , hull2
  , hull3
  , hull4
  , squaredMagnitude
  , magnitude
  , maxMagnitude
  , maxSquaredMagnitude
  , normalize
  , interpolate
  )
where

import CoordinateSystem (Units)
import Generic qualified
import OpenSolid
import Qty qualified
import Range (Range (..))
import Range qualified
import Units (Unitless)
import Units qualified
import Vector3d (Vector3d (..))
import Vector3d qualified

data VectorBox3d (coordinateSystem :: CoordinateSystem)
  = VectorBox3d
      (Range (Units coordinateSystem))
      (Range (Units coordinateSystem))
      (Range (Units coordinateSystem))
  deriving (Show)

instance
  (units1 ~ units1', units2 ~ units2', space ~ space')
  => Units.Coercion
      units1
      units2
      (VectorBox3d (space @ units1'))
      (VectorBox3d (space' @ units2'))

instance Generic.Zero (VectorBox3d (space @ units)) where
  zero = constant Vector3d.zero

instance Negation (VectorBox3d (space @ units)) where
  negate (VectorBox3d x y z) = VectorBox3d (negate x) (negate y) (negate z)

instance (units ~ units', space ~ space') => Addition (VectorBox3d (space @ units)) (VectorBox3d (space' @ units')) (VectorBox3d (space @ units)) where
  VectorBox3d x1 y1 z1 + VectorBox3d x2 y2 z2 = VectorBox3d (x1 + x2) (y1 + y2) (z1 + z2)

instance (units ~ units', space ~ space') => Addition (VectorBox3d (space @ units)) (Vector3d (space' @ units')) (VectorBox3d (space @ units)) where
  VectorBox3d x1 y1 z1 + Vector3d x2 y2 z2 = VectorBox3d (x1 + x2) (y1 + y2) (z1 + z2)

instance (units ~ units', space ~ space') => Addition (Vector3d (space @ units)) (VectorBox3d (space' @ units')) (VectorBox3d (space @ units)) where
  Vector3d x1 y1 z1 + VectorBox3d x2 y2 z2 = VectorBox3d (x1 + x2) (y1 + y2) (z1 + z2)

instance (units ~ units', space ~ space') => Subtraction (VectorBox3d (space @ units)) (VectorBox3d (space' @ units')) (VectorBox3d (space @ units)) where
  VectorBox3d x1 y1 z1 - VectorBox3d x2 y2 z2 = VectorBox3d (x1 - x2) (y1 - y2) (z1 - z2)

instance (units ~ units', space ~ space') => Subtraction (VectorBox3d (space @ units)) (Vector3d (space' @ units')) (VectorBox3d (space @ units)) where
  VectorBox3d x1 y1 z1 - Vector3d x2 y2 z2 = VectorBox3d (x1 - x2) (y1 - y2) (z1 - z2)

instance (units ~ units', space ~ space') => Subtraction (Vector3d (space @ units)) (VectorBox3d (space' @ units')) (VectorBox3d (space @ units)) where
  Vector3d x1 y1 z1 - VectorBox3d x2 y2 z2 = VectorBox3d (x1 - x2) (y1 - y2) (z1 - z2)

instance Units.Product units1 units2 units3 => Multiplication (Qty units1) (VectorBox3d (space @ units2)) (VectorBox3d (space @ units3)) where
  value * VectorBox3d x y z = VectorBox3d (value * x) (value * y) (value * z)

instance Units.Product units1 units2 units3 => Multiplication (VectorBox3d (space @ units1)) (Qty units2) (VectorBox3d (space @ units3)) where
  VectorBox3d x y z * value = VectorBox3d (x * value) (y * value) (z * value)

instance Units.Product units1 units2 units3 => Multiplication (Range units1) (VectorBox3d (space @ units2)) (VectorBox3d (space @ units3)) where
  range * VectorBox3d x y z = VectorBox3d (range * x) (range * y) (range * z)

instance Units.Product units1 units2 units3 => Multiplication (VectorBox3d (space @ units1)) (Range units2) (VectorBox3d (space @ units3)) where
  VectorBox3d x y z * range = VectorBox3d (x * range) (y * range) (z * range)

instance Units.Quotient units1 units2 units3 => Division (VectorBox3d (space @ units1)) (Qty units2) (VectorBox3d (space @ units3)) where
  VectorBox3d x y z / value = VectorBox3d (x / value) (y / value) (z / value)

instance Units.Quotient units1 units2 units3 => Division (VectorBox3d (space @ units1)) (Range units2) (VectorBox3d (space @ units3)) where
  VectorBox3d x y z / range = VectorBox3d (x / range) (y / range) (z / range)

instance
  (Units.Product units1 units2 units3, space ~ space')
  => DotProduct
      (Vector3d (space @ units1))
      (VectorBox3d (space' @ units2))
      (Range units3)
  where
  Vector3d x1 y1 z1 <> VectorBox3d x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

instance
  (Units.Product units1 units2 units3, space ~ space')
  => DotProduct
      (VectorBox3d (space @ units1))
      (Vector3d (space' @ units2))
      (Range units3)
  where
  VectorBox3d x1 y1 z1 <> Vector3d x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

instance
  (Units.Product units1 units2 units3, space ~ space')
  => DotProduct
      (VectorBox3d (space @ units1))
      (VectorBox3d (space' @ units2))
      (Range units3)
  where
  VectorBox3d x1 y1 z1 <> VectorBox3d x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

instance
  (Units.Product units1 units2 units3, space ~ space')
  => CrossProduct
      (Vector3d (space @ units1))
      (VectorBox3d (space' @ units2))
      (VectorBox3d (space @ units3))
  where
  Vector3d x1 y1 z1 >< VectorBox3d x2 y2 z2 =
    VectorBox3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

instance
  (Units.Product units1 units2 units3, space ~ space')
  => CrossProduct
      (VectorBox3d (space @ units1))
      (Vector3d (space' @ units2))
      (VectorBox3d (space @ units3))
  where
  VectorBox3d x1 y1 z1 >< Vector3d x2 y2 z2 =
    VectorBox3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

instance
  (Units.Product units1 units2 units3, space ~ space')
  => CrossProduct
      (VectorBox3d (space @ units1))
      (VectorBox3d (space' @ units2))
      (VectorBox3d (space @ units3))
  where
  VectorBox3d x1 y1 z1 >< VectorBox3d x2 y2 z2 =
    VectorBox3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

constant :: Vector3d (space @ units) -> VectorBox3d (space @ units)
constant (Vector3d x y z) = VectorBox3d (Range.constant x) (Range.constant y) (Range.constant z)

hull2 :: Vector3d (space @ units) -> Vector3d (space @ units) -> VectorBox3d (space @ units)
hull2 (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) =
  VectorBox3d (Range.from x1 x2) (Range.from y1 y2) (Range.from z1 z2)

hull3
  :: Vector3d (space @ units)
  -> Vector3d (space @ units)
  -> Vector3d (space @ units)
  -> VectorBox3d (space @ units)
hull3 (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) (Vector3d x3 y3 z3) =
  let minX = min (min x1 x2) x3
      maxX = max (max x1 x2) x3
      minY = min (min y1 y2) y3
      maxY = max (max y1 y2) y3
      minZ = min (min z1 z2) z3
      maxZ = max (max z1 z2) z3
   in VectorBox3d (Range.unsafe minX maxX) (Range.unsafe minY maxY) (Range.unsafe minZ maxZ)

hull4
  :: Vector3d (space @ units)
  -> Vector3d (space @ units)
  -> Vector3d (space @ units)
  -> Vector3d (space @ units)
  -> VectorBox3d (space @ units)
hull4 (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) (Vector3d x3 y3 z3) (Vector3d x4 y4 z4) =
  let minX = min (min (min x1 x2) x3) x4
      maxX = max (max (max x1 x2) x3) x4
      minY = min (min (min y1 y2) y3) y4
      maxY = max (max (max y1 y2) y3) y4
      minZ = min (min (min z1 z2) z3) z4
      maxZ = max (max (max z1 z2) z3) z4
   in VectorBox3d (Range.unsafe minX maxX) (Range.unsafe minY maxY) (Range.unsafe minZ maxZ)

squaredMagnitude :: Units.Squared units1 units2 => VectorBox3d (space @ units1) -> Range units2
squaredMagnitude (VectorBox3d x y z) = Range.squared x + Range.squared y + Range.squared z

magnitude :: VectorBox3d (space @ units) -> Range units
magnitude (VectorBox3d x y z) = Range.hypot3 x y z

maxMagnitude :: VectorBox3d (space @ units) -> Qty units
maxMagnitude (VectorBox3d x y z) =
  let xMagnitude = max (Qty.abs x.minValue) (Qty.abs x.maxValue)
      yMagnitude = max (Qty.abs y.minValue) (Qty.abs y.maxValue)
      zMagnitude = max (Qty.abs z.minValue) (Qty.abs z.maxValue)
   in Qty.hypot3 xMagnitude yMagnitude zMagnitude

maxSquaredMagnitude :: Units.Squared units1 units2 => VectorBox3d (space @ units1) -> Qty units2
maxSquaredMagnitude (VectorBox3d x y z) =
  let xMagnitude = max (Qty.abs x.minValue) (Qty.abs x.maxValue)
      yMagnitude = max (Qty.abs y.minValue) (Qty.abs y.maxValue)
      zMagnitude = max (Qty.abs z.minValue) (Qty.abs z.maxValue)
   in Qty.squared xMagnitude + Qty.squared yMagnitude + Qty.squared zMagnitude

normalize :: VectorBox3d (space @ units) -> VectorBox3d (space @ Unitless)
normalize vectorBox =
  let (VectorBox3d x y z) = vectorBox / magnitude vectorBox
      nx = clampNormalized x
      ny = clampNormalized y
      nz = clampNormalized z
   in VectorBox3d nx ny nz

clampNormalized :: Range Unitless -> Range Unitless
clampNormalized (Range low high) =
  Range.unsafe
    (Qty.clamp -1.0 1.0 low)
    (Qty.clamp -1.0 1.0 high)

interpolate :: VectorBox3d (space @ units) -> Float -> Float -> Float -> Vector3d (space @ units)
interpolate (VectorBox3d x y z) u v w =
  Vector3d (Range.interpolate x u) (Range.interpolate y v) (Range.interpolate z w)
