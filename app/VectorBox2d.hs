module VectorBox2d (
    VectorBox2d (..),
    constant,
    hull2,
    hull3,
    hull4,
    squaredMagnitude,
    magnitude,
    normalize,
) where

import Generic qualified
import OpenSolid
import Qty qualified
import Range (Range)
import Range qualified
import Units qualified
import Vector2d (Vector2d (..))
import Vector2d qualified

data VectorBox2d units coordinates = VectorBox2d !(Range units) !(Range units)

instance Units.Coercion (VectorBox2d units coordinates) (VectorBox2d Unitless coordinates)

instance Generic.Zero (VectorBox2d units) where
    zero = constant Vector2d.zero

instance Negation (VectorBox2d units coordinates) where
    negate (VectorBox2d x y) = VectorBox2d (negate x) (negate y)

instance Addition (VectorBox2d units) (VectorBox2d units) (VectorBox2d units) where
    (VectorBox2d x1 y1) + (VectorBox2d x2 y2) = VectorBox2d (x1 + x2) (y1 + y2)

instance Subtraction (VectorBox2d units) (VectorBox2d units) (VectorBox2d units) where
    (VectorBox2d x1 y1) - (VectorBox2d x2 y2) = VectorBox2d (x1 - x2) (y1 - y2)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Qty units1) (VectorBox2d units2 coordinates) (VectorBox2d units3 coordinates) where
    value * (VectorBox2d x y) = VectorBox2d (value * x) (value * y)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (VectorBox2d units1 coordinates) (Qty units2) (VectorBox2d units3 coordinates) where
    (VectorBox2d x y) * value = VectorBox2d (x * value) (y * value)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Range units1) (VectorBox2d units2 coordinates) (VectorBox2d units3 coordinates) where
    range * (VectorBox2d x y) = VectorBox2d (range * x) (range * y)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (VectorBox2d units1 coordinates) (Range units2) (VectorBox2d units3 coordinates) where
    (VectorBox2d x y) * range = VectorBox2d (x * range) (y * range)

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (VectorBox2d units1 coordinates) (Qty units2) (VectorBox2d units3 coordinates) where
    (VectorBox2d x y) / value = VectorBox2d (x / value) (y / value)

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (VectorBox2d units1 coordinates) (Range units2) (VectorBox2d units3 coordinates) where
    (VectorBox2d x y) / range = VectorBox2d (x / range) (y / range)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => DotProduct (Vector2d units1) (VectorBox2d units2) (Range units3) where
    (Vector2d x1 y1) <> (VectorBox2d x2 y2) = x1 * x2 + y1 * y2

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => DotProduct (VectorBox2d units1) (Vector2d units2) (Range units3) where
    (VectorBox2d x1 y1) <> (Vector2d x2 y2) = x1 * x2 + y1 * y2

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => DotProduct (VectorBox2d units1) (VectorBox2d units2) (Range units3) where
    (VectorBox2d x1 y1) <> (VectorBox2d x2 y2) = x1 * x2 + y1 * y2

constant :: Vector2d units coordinates -> VectorBox2d units coordinates
constant (Vector2d x y) = VectorBox2d (Range.constant x) (Range.constant y)

hull2 :: Vector2d units coordinates -> Vector2d units coordinates -> VectorBox2d units coordinates
hull2 (Vector2d x1 y1) (Vector2d x2 y2) = VectorBox2d (Range.from x1 x2) (Range.from y1 y2)

hull3 :: Vector2d units coordinates -> Vector2d units coordinates -> Vector2d units coordinates -> VectorBox2d units coordinates
hull3 (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) =
    let minX = min (min x1 x2) x3
        maxX = max (max x1 x2) x3
        minY = min (min y1 y2) y3
        maxY = max (max y1 y2) y3
     in VectorBox2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

hull4 :: Vector2d units coordinates -> Vector2d units coordinates -> Vector2d units coordinates -> Vector2d units coordinates -> VectorBox2d units coordinates
hull4 (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) (Vector2d x4 y4) =
    let minX = min (min (min x1 x2) x3) x4
        maxX = max (max (max x1 x2) x3) x4
        minY = min (min (min y1 y2) y3) y4
        maxY = max (max (max y1 y2) y3) y4
     in VectorBox2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

squaredMagnitude :: Squared (Qty units1) (Qty units2) => VectorBox2d units1 coordinates -> Range units2
squaredMagnitude (VectorBox2d x y) = Range.squared x + Range.squared y

magnitude :: VectorBox2d units coordinates -> Range units
magnitude vectorBox =
    let (VectorBox2d x y) = Units.drop vectorBox
     in Units.add (Range.sqrt (Range.squared x + Range.squared y))

normalize :: VectorBox2d units coordinates -> VectorBox2d Unitless coordinates
normalize vectorBox =
    let (VectorBox2d x y) = vectorBox / magnitude vectorBox
        nx = clampNormalized x
        ny = clampNormalized y
     in VectorBox2d nx ny

clampNormalized :: Range Unitless -> Range Unitless
clampNormalized range =
    Range.unsafe
        (Qty.clamp -1.0 1.0 (Range.minValue range))
        (Qty.clamp -1.0 1.0 (Range.maxValue range))
