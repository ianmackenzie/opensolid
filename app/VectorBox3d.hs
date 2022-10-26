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

import qualified Float
import OpenSolid
import qualified Range
import Range.Unsafe
import qualified Units
import Vector3d (Vector3d (..))

data VectorBox3d scalar coordinates = VectorBox3d !(Range scalar) !(Range scalar) !(Range scalar)

instance Units.Coercion (VectorBox3d scalar coordinates) (VectorBox3d Float coordinates)

instance Scalar scalar => Negation (VectorBox3d scalar coordinates) where
    negate (VectorBox3d x y z) =
        VectorBox3d (negate x) (negate y) (negate z)

instance Scalar scalar => Addition (VectorBox3d scalar coordinates) where
    (VectorBox3d x1 y1 z1) + (VectorBox3d x2 y2 z2) =
        VectorBox3d (x1 + x2) (y1 + y2) (z1 + z2)

instance Scalar scalar => Subtraction (VectorBox3d scalar coordinates) where
    (VectorBox3d x1 y1 z1) - (VectorBox3d x2 y2 z2) =
        VectorBox3d (x1 - x2) (y1 - y2) (z1 - z2)

instance Scalar scalar => Multiplication Float (VectorBox3d scalar coordinates) (VectorBox3d scalar coordinates) where
    value * (VectorBox3d x y z) =
        VectorBox3d (value * x) (value * y) (value * z)

instance (Scalar scalar, Scalar result, Multiplication (Quantity units) scalar result) => Multiplication (Quantity units) (VectorBox3d scalar coordinates) (VectorBox3d result coordinates) where
    value * (VectorBox3d x y z) =
        VectorBox3d (value * x) (value * y) (value * z)

instance Scalar scalar => Multiplication (VectorBox3d scalar coordinates) Float (VectorBox3d scalar coordinates) where
    (VectorBox3d x y z) * value =
        VectorBox3d (value * x) (value * y) (value * z)

instance (Scalar scalar, Scalar result, Multiplication (Quantity units) scalar result) => Multiplication (VectorBox3d scalar coordinates) (Quantity units) (VectorBox3d result coordinates) where
    (VectorBox3d x y z) * value =
        VectorBox3d (value * x) (value * y) (value * z)

instance (Scalar scalar1, Scalar scalar2, Scalar result, Multiplication scalar1 scalar2 result) => Multiplication (Range scalar1) (VectorBox3d scalar2 coordinates) (VectorBox3d result coordinates) where
    range * (VectorBox3d x y z) =
        VectorBox3d (range * x) (range * y) (range * z)

instance (Scalar scalar1, Scalar scalar2, Scalar result, Multiplication scalar1 scalar2 result) => Multiplication (VectorBox3d scalar1 coordinates) (Range scalar2) (VectorBox3d result coordinates) where
    (VectorBox3d x y z) * range =
        VectorBox3d (x * range) (y * range) (z * range)

instance Scalar scalar => Division (VectorBox3d scalar coordinates) Float (VectorBox3d scalar coordinates) where
    (VectorBox3d x y z) / scalar =
        VectorBox3d (x / scalar) (y / scalar) (z / scalar)

instance (Scalar scalar, Scalar result, Division scalar (Quantity units) result) => Division (VectorBox3d scalar coordinates) (Quantity units) (VectorBox3d result coordinates) where
    (VectorBox3d x y z) / scalar =
        VectorBox3d (x / scalar) (y / scalar) (z / scalar)

instance (Scalar scalar1, Scalar scalar2, Scalar result, Division scalar1 scalar2 result) => Division (VectorBox3d scalar1 coordinates) (Range scalar2) (VectorBox3d result coordinates) where
    (VectorBox3d x y z) / range =
        VectorBox3d (x / range) (y / range) (z / range)

instance Scalar scalar => DotProduct (Vector3d Float) (VectorBox3d scalar) (Range scalar) where
    (Vector3d x1 y1 z1) <> (VectorBox3d x2 y2 z2) =
        x1 * x2 + y1 * y2 + z1 * z2

instance (Scalar scalar, Scalar result, Multiplication (Quantity units) scalar result) => DotProduct (Vector3d (Quantity units)) (VectorBox3d scalar) (Range result) where
    (Vector3d x1 y1 z1) <> (VectorBox3d x2 y2 z2) =
        x1 * x2 + y1 * y2 + z1 * z2

instance Scalar scalar => DotProduct (VectorBox3d scalar) (Vector3d Float) (Range scalar) where
    (VectorBox3d x1 y1 z1) <> (Vector3d x2 y2 z2) =
        x1 * x2 + y1 * y2 + z1 * z2

instance (Scalar scalar, Scalar result, Multiplication (Quantity units) scalar result) => DotProduct (VectorBox3d scalar) (Vector3d (Quantity units)) (Range result) where
    (VectorBox3d x1 y1 z1) <> (Vector3d x2 y2 z2) =
        x1 * x2 + y1 * y2 + z1 * z2

instance (Scalar scalar1, Scalar scalar2, Scalar result, Multiplication scalar1 scalar2 result) => DotProduct (VectorBox3d scalar1) (VectorBox3d scalar2) (Range result) where
    (VectorBox3d x1 y1 z1) <> (VectorBox3d x2 y2 z2) =
        x1 * x2 + y1 * y2 + z1 * z2

instance Scalar scalar => CrossProduct (Vector3d Float) (VectorBox3d scalar) (VectorBox3d scalar) where
    (Vector3d x1 y1 z1) >< (VectorBox3d x2 y2 z2) =
        let x = y1 * z2 - z1 * y2
            y = z1 * x2 - x1 * z2
            z = x1 * y2 - y1 * x2
         in VectorBox3d x y z

instance (Scalar scalar, Scalar result, Multiplication (Quantity units) scalar result) => CrossProduct (Vector3d (Quantity units)) (VectorBox3d scalar) (VectorBox3d result) where
    (Vector3d x1 y1 z1) >< (VectorBox3d x2 y2 z2) =
        let x = y1 * z2 - z1 * y2
            y = z1 * x2 - x1 * z2
            z = x1 * y2 - y1 * x2
         in VectorBox3d x y z

instance Scalar scalar => CrossProduct (VectorBox3d scalar) (Vector3d Float) (VectorBox3d scalar) where
    (VectorBox3d x1 y1 z1) >< (Vector3d x2 y2 z2) =
        let x = y1 * z2 - z1 * y2
            y = z1 * x2 - x1 * z2
            z = x1 * y2 - y1 * x2
         in VectorBox3d x y z

instance (Scalar scalar, Scalar result, Multiplication (Quantity units) scalar result) => CrossProduct (VectorBox3d scalar) (Vector3d (Quantity units)) (VectorBox3d result) where
    (VectorBox3d x1 y1 z1) >< (Vector3d x2 y2 z2) =
        let x = y1 * z2 - z1 * y2
            y = z1 * x2 - x1 * z2
            z = x1 * y2 - y1 * x2
         in VectorBox3d x y z

instance (Scalar scalar1, Scalar scalar2, Scalar result, Multiplication scalar1 scalar2 result) => CrossProduct (VectorBox3d scalar1) (VectorBox3d scalar2) (VectorBox3d result) where
    (VectorBox3d x1 y1 z1) >< (VectorBox3d x2 y2 z2) =
        let x = y1 * z2 - z1 * y2
            y = z1 * x2 - x1 * z2
            z = x1 * y2 - y1 * x2
         in VectorBox3d x y z

constant :: Scalar scalar => Vector3d scalar coordinates -> VectorBox3d scalar coordinates
constant vector =
    let (Vector3d x y z) = vector
     in VectorBox3d (Range.constant x) (Range.constant y) (Range.constant z)

hull2 :: Scalar scalar => Vector3d scalar coordinates -> Vector3d scalar coordinates -> VectorBox3d scalar coordinates
hull2 v1 v2 =
    let (Vector3d x1 y1 z1) = v1
        (Vector3d x2 y2 z2) = v2
     in VectorBox3d (Range.from x1 x2) (Range.from y1 y2) (Range.from z1 z2)

hull3 :: Scalar scalar => Vector3d scalar coordinates -> Vector3d scalar coordinates -> Vector3d scalar coordinates -> VectorBox3d scalar coordinates
hull3 v1 v2 v3 =
    let (Vector3d x1 y1 z1) = v1
        (Vector3d x2 y2 z2) = v2
        (Vector3d x3 y3 z3) = v3
        minX = min (min x1 x2) x3
        maxX = max (max x1 x2) x3
        minY = min (min y1 y2) y3
        maxY = max (max y1 y2) y3
        minZ = min (min z1 z2) z3
        maxZ = max (max z1 z2) z3
     in VectorBox3d (Range minX maxX) (Range minY maxY) (Range minZ maxZ)

hull4 :: Scalar scalar => Vector3d scalar coordinates -> Vector3d scalar coordinates -> Vector3d scalar coordinates -> Vector3d scalar coordinates -> VectorBox3d scalar coordinates
hull4 v1 v2 v3 v4 =
    let (Vector3d x1 y1 z1) = v1
        (Vector3d x2 y2 z2) = v2
        (Vector3d x3 y3 z3) = v3
        (Vector3d x4 y4 z4) = v4
        minX = min (min (min x1 x2) x3) x4
        maxX = max (max (max x1 x2) x3) x4
        minY = min (min (min y1 y2) y3) y4
        maxY = max (max (max y1 y2) y3) y4
        minZ = min (min (min z1 z2) z3) z4
        maxZ = max (max (max z1 z2) z3) z4
     in VectorBox3d (Range minX maxX) (Range minY maxY) (Range minZ maxZ)

squaredMagnitude :: (Scalar scalar, Scalar squaredScalar, Multiplication scalar scalar squaredScalar) => VectorBox3d scalar coordinates -> Range squaredScalar
squaredMagnitude vectorBox =
    let (VectorBox3d x y z) = vectorBox
     in Range.squared x + Range.squared y + Range.squared z

magnitude :: Scalar scalar => VectorBox3d scalar coordinates -> Range scalar
magnitude vectorBox =
    let (VectorBox3d x y z) = Units.drop vectorBox
     in Units.add (Range.sqrt (Range.squared x + Range.squared y + Range.squared z))

normalize :: Scalar scalar => VectorBox3d scalar coordinates -> VectorBox3d Float coordinates
normalize vectorBox =
    let (VectorBox3d x y z) = vectorBox / magnitude vectorBox
        nx = clampNormalized x
        ny = clampNormalized y
        nz = clampNormalized z
     in VectorBox3d nx ny nz

clampNormalized :: Range Float -> Range Float
clampNormalized (Range low high) =
    Range (Float.clamp (-1.0) 1.0 low) (Float.clamp (-1.0) 1.0 high)
