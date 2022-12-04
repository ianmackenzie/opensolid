module VectorBox3d (
    VectorBox3d (..),
    constant,
    hull2,
    hull3,
    hull4,
    squaredMagnitude,
    magnitude,
    normalize,
) where

import OpenSolid
import Range (Range)
import qualified Range
import qualified Units
import Vector3d (Vector3d (..))

data VectorBox3d units coordinates = VectorBox3d !(Range units) !(Range units) !(Range units)

instance Units.Coercion (VectorBox3d units coordinates) (VectorBox3d Unitless coordinates)

instance Negation (VectorBox3d units coordinates) where
    negate (VectorBox3d x y z) = VectorBox3d (negate x) (negate y) (negate z)

instance Addition (VectorBox3d units) (VectorBox3d units) (VectorBox3d units) where
    (VectorBox3d x1 y1 z1) + (VectorBox3d x2 y2 z2) = VectorBox3d (x1 + x2) (y1 + y2) (z1 + z2)

instance Subtraction (VectorBox3d units) (VectorBox3d units) (VectorBox3d units) where
    (VectorBox3d x1 y1 z1) - (VectorBox3d x2 y2 z2) = VectorBox3d (x1 - x2) (y1 - y2) (z1 - z2)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Qty units1) (VectorBox3d units2 coordinates) (VectorBox3d units3 coordinates) where
    value * (VectorBox3d x y z) = VectorBox3d (value * x) (value * y) (value * z)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (VectorBox3d units1 coordinates) (Qty units2) (VectorBox3d units3 coordinates) where
    (VectorBox3d x y z) * value = VectorBox3d (x * value) (y * value) (z * value)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Range units1) (VectorBox3d units2 coordinates) (VectorBox3d units3 coordinates) where
    range * (VectorBox3d x y z) = VectorBox3d (range * x) (range * y) (range * z)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (VectorBox3d units1 coordinates) (Range units2) (VectorBox3d units3 coordinates) where
    (VectorBox3d x y z) * range = VectorBox3d (x * range) (y * range) (z * range)

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (VectorBox3d units1 coordinates) (Qty units2) (VectorBox3d units3 coordinates) where
    (VectorBox3d x y z) / value = VectorBox3d (x / value) (y / value) (z / value)

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (VectorBox3d units1 coordinates) (Range units2) (VectorBox3d units3 coordinates) where
    (VectorBox3d x y z) / range = VectorBox3d (x / range) (y / range) (z / range)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => DotProduct (Vector3d units1) (VectorBox3d units2) (Range units3) where
    (Vector3d x1 y1 z1) <> (VectorBox3d x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => DotProduct (VectorBox3d units1) (Vector3d units2) (Range units3) where
    (VectorBox3d x1 y1 z1) <> (Vector3d x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => DotProduct (VectorBox3d units1) (VectorBox3d units2) (Range units3) where
    (VectorBox3d x1 y1 z1) <> (VectorBox3d x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => CrossProduct (Vector3d units1) (VectorBox3d units2) (VectorBox3d units3) where
    (Vector3d x1 y1 z1) >< (VectorBox3d x2 y2 z2) =
        VectorBox3d
            (y1 * z2 - z1 * y2)
            (z1 * x2 - x1 * z2)
            (x1 * y2 - y1 * x2)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => CrossProduct (VectorBox3d units1) (Vector3d units2) (VectorBox3d units3) where
    (VectorBox3d x1 y1 z1) >< (Vector3d x2 y2 z2) =
        VectorBox3d
            (y1 * z2 - z1 * y2)
            (z1 * x2 - x1 * z2)
            (x1 * y2 - y1 * x2)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => CrossProduct (VectorBox3d units1) (VectorBox3d units2) (VectorBox3d units3) where
    (VectorBox3d x1 y1 z1) >< (VectorBox3d x2 y2 z2) =
        VectorBox3d
            (y1 * z2 - z1 * y2)
            (z1 * x2 - x1 * z2)
            (x1 * y2 - y1 * x2)

constant :: Vector3d units coordinates -> VectorBox3d units coordinates
constant (Vector3d x y z) = VectorBox3d (Range.constant x) (Range.constant y) (Range.constant z)

hull2 :: Vector3d units coordinates -> Vector3d units coordinates -> VectorBox3d units coordinates
hull2 (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) = VectorBox3d (Range.from x1 x2) (Range.from y1 y2) (Range.from z1 z2)

hull3 :: Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates -> VectorBox3d units coordinates
hull3 (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) (Vector3d x3 y3 z3) =
    let minX = min (min x1 x2) x3
        maxX = max (max x1 x2) x3
        minY = min (min y1 y2) y3
        maxY = max (max y1 y2) y3
        minZ = min (min z1 z2) z3
        maxZ = max (max z1 z2) z3
     in VectorBox3d (Range.unsafe minX maxX) (Range.unsafe minY maxY) (Range.unsafe minZ maxZ)

hull4 :: Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates -> VectorBox3d units coordinates
hull4 (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) (Vector3d x3 y3 z3) (Vector3d x4 y4 z4) =
    let minX = min (min (min x1 x2) x3) x4
        maxX = max (max (max x1 x2) x3) x4
        minY = min (min (min y1 y2) y3) y4
        maxY = max (max (max y1 y2) y3) y4
        minZ = min (min (min z1 z2) z3) z4
        maxZ = max (max (max z1 z2) z3) z4
     in VectorBox3d (Range.unsafe minX maxX) (Range.unsafe minY maxY) (Range.unsafe minZ maxZ)

squaredMagnitude :: Multiplication (Qty units1) (Qty units1) (Qty units2) => VectorBox3d units1 coordinates -> Range units2
squaredMagnitude (VectorBox3d x y z) = Range.squared x + Range.squared y + Range.squared z

magnitude :: VectorBox3d units coordinates -> Range units
magnitude vectorBox =
    let (VectorBox3d x y z) = Units.drop vectorBox
     in Units.add (Range.sqrt (Range.squared x + Range.squared y + Range.squared z))

normalize :: VectorBox3d units coordinates -> VectorBox3d Unitless coordinates
normalize vectorBox =
    let (VectorBox3d x y z) = vectorBox / magnitude vectorBox
        nx = clampNormalized x
        ny = clampNormalized y
        nz = clampNormalized z
     in VectorBox3d nx ny nz

clampNormalized :: Range Unitless -> Range Unitless
clampNormalized range =
    Range.unsafe
        (clamp (-1.0) 1.0 (Range.minValue range))
        (clamp (-1.0) 1.0 (Range.maxValue range))
