module VectorBox2d
  ( VectorBox2d (..)
  , constant
  , hull2
  , hull3
  , hull4
  , polar
  , squaredMagnitude
  , magnitude
  , normalize
  , interpolate
  )
where

import Generic qualified
import OpenSolid
import Qty qualified
import Range (Range (..))
import Range qualified
import Units (Radians, Unitless)
import Units qualified
import Vector2d (Vector2d (..))
import Vector2d qualified

type role VectorBox2d nominal nominal

type VectorBox2d :: Type -> Type -> Type
data VectorBox2d coordinates units = VectorBox2d (Range units) (Range units) deriving (Show)

instance Units.Coercion (VectorBox2d coordinates)

instance Generic.Zero (VectorBox2d coordinates units) where
  zero = constant Vector2d.zero

instance Negation (VectorBox2d coordinates units) where
  negate (VectorBox2d x y) = VectorBox2d (negate x) (negate y)

instance (units ~ units', coordinates ~ coordinates') => Addition (VectorBox2d coordinates units) (VectorBox2d coordinates' units') (VectorBox2d coordinates units) where
  VectorBox2d x1 y1 + VectorBox2d x2 y2 = VectorBox2d (x1 + x2) (y1 + y2)

instance (units ~ units', coordinates ~ coordinates') => Addition (VectorBox2d coordinates units) (Vector2d coordinates' units') (VectorBox2d coordinates units) where
  VectorBox2d x1 y1 + Vector2d x2 y2 = VectorBox2d (x1 + x2) (y1 + y2)

instance (units ~ units', coordinates ~ coordinates') => Addition (Vector2d coordinates units) (VectorBox2d coordinates' units') (VectorBox2d coordinates units) where
  Vector2d x1 y1 + VectorBox2d x2 y2 = VectorBox2d (x1 + x2) (y1 + y2)

instance (units ~ units', coordinates ~ coordinates') => Subtraction (VectorBox2d coordinates units) (VectorBox2d coordinates' units') (VectorBox2d coordinates units) where
  VectorBox2d x1 y1 - VectorBox2d x2 y2 = VectorBox2d (x1 - x2) (y1 - y2)

instance (units ~ units', coordinates ~ coordinates') => Subtraction (VectorBox2d coordinates units) (Vector2d coordinates' units') (VectorBox2d coordinates units) where
  VectorBox2d x1 y1 - Vector2d x2 y2 = VectorBox2d (x1 - x2) (y1 - y2)

instance (units ~ units', coordinates ~ coordinates') => Subtraction (Vector2d coordinates units) (VectorBox2d coordinates' units') (VectorBox2d coordinates units) where
  Vector2d x1 y1 - VectorBox2d x2 y2 = VectorBox2d (x1 - x2) (y1 - y2)

instance Units.Product units1 units2 units3 => Multiplication (Qty units1) (VectorBox2d coordinates units2) (VectorBox2d coordinates units3) where
  value * VectorBox2d x y = VectorBox2d (value * x) (value * y)

instance Units.Product units1 units2 units3 => Multiplication (VectorBox2d coordinates units1) (Qty units2) (VectorBox2d coordinates units3) where
  VectorBox2d x y * value = VectorBox2d (x * value) (y * value)

instance Units.Product units1 units2 units3 => Multiplication (Range units1) (VectorBox2d coordinates units2) (VectorBox2d coordinates units3) where
  range * VectorBox2d x y = VectorBox2d (range * x) (range * y)

instance Units.Product units1 units2 units3 => Multiplication (VectorBox2d coordinates units1) (Range units2) (VectorBox2d coordinates units3) where
  VectorBox2d x y * range = VectorBox2d (x * range) (y * range)

instance Units.Quotient units1 units2 units3 => Division (VectorBox2d coordinates units1) (Qty units2) (VectorBox2d coordinates units3) where
  VectorBox2d x y / value = VectorBox2d (x / value) (y / value)

instance Units.Quotient units1 units2 units3 => Division (VectorBox2d coordinates units1) (Range units2) (VectorBox2d coordinates units3) where
  VectorBox2d x y / range = VectorBox2d (x / range) (y / range)

instance (Units.Product units1 units2 units3, coordinates ~ coordinates') => DotProduct (Vector2d coordinates units1) (VectorBox2d coordinates' units2) (Range units3) where
  Vector2d x1 y1 <> VectorBox2d x2 y2 = x1 * x2 + y1 * y2

instance (Units.Product units1 units2 units3, coordinates ~ coordinates') => DotProduct (VectorBox2d coordinates units1) (Vector2d coordinates' units2) (Range units3) where
  VectorBox2d x1 y1 <> Vector2d x2 y2 = x1 * x2 + y1 * y2

instance (Units.Product units1 units2 units3, coordinates ~ coordinates') => DotProduct (VectorBox2d coordinates units1) (VectorBox2d coordinates' units2) (Range units3) where
  VectorBox2d x1 y1 <> VectorBox2d x2 y2 = x1 * x2 + y1 * y2

constant :: Vector2d coordinates units -> VectorBox2d coordinates units
constant (Vector2d x y) = VectorBox2d (Range.constant x) (Range.constant y)

hull2 :: Vector2d coordinates units -> Vector2d coordinates units -> VectorBox2d coordinates units
hull2 (Vector2d x1 y1) (Vector2d x2 y2) = VectorBox2d (Range.from x1 x2) (Range.from y1 y2)

hull3
  :: Vector2d coordinates units
  -> Vector2d coordinates units
  -> Vector2d coordinates units
  -> VectorBox2d coordinates units
hull3 (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) =
  let minX = min (min x1 x2) x3
      maxX = max (max x1 x2) x3
      minY = min (min y1 y2) y3
      maxY = max (max y1 y2) y3
   in VectorBox2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

hull4
  :: Vector2d coordinates units
  -> Vector2d coordinates units
  -> Vector2d coordinates units
  -> Vector2d coordinates units
  -> VectorBox2d coordinates units
hull4 (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) (Vector2d x4 y4) =
  let minX = min (min (min x1 x2) x3) x4
      maxX = max (max (max x1 x2) x3) x4
      minY = min (min (min y1 y2) y3) y4
      maxY = max (max (max y1 y2) y3) y4
   in VectorBox2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

polar :: Range units -> Range Radians -> VectorBox2d coordinates units
polar r theta = VectorBox2d (r * Range.cos theta) (r * Range.sin theta)

squaredMagnitude :: Units.Squared units1 units2 => VectorBox2d coordinates units1 -> Range units2
squaredMagnitude (VectorBox2d x y) = Range.squared x + Range.squared y

magnitude :: VectorBox2d coordinates units -> Range units
magnitude (VectorBox2d x y) = Range.hypot2 x y

normalize :: VectorBox2d coordinates units -> VectorBox2d coordinates Unitless
normalize vectorBox =
  let (VectorBox2d x y) = vectorBox / magnitude vectorBox
      nx = clampNormalized x
      ny = clampNormalized y
   in VectorBox2d nx ny

clampNormalized :: Range Unitless -> Range Unitless
clampNormalized (Range low high) =
  Range.unsafe
    (Qty.clamp -1.0 1.0 low)
    (Qty.clamp -1.0 1.0 high)

interpolate :: VectorBox2d coordinates units -> Float -> Float -> Vector2d coordinates units
interpolate (VectorBox2d x y) u v =
  Vector2d (Range.interpolate x u) (Range.interpolate y v)
