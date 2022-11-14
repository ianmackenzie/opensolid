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

data VectorBox3d qty coordinates = VectorBox3d !(Range qty) !(Range qty) !(Range qty)

instance Units.Coercion (VectorBox3d (Qty a) coordinates) (VectorBox3d Float coordinates)

instance Negation (VectorBox3d (Qty a) coordinates) where
    negate (VectorBox3d x y z) =
        VectorBox3d (negate x) (negate y) (negate z)

instance Addition (VectorBox3d (Qty a)) (VectorBox3d (Qty a)) (VectorBox3d (Qty a)) coordinates where
    (VectorBox3d x1 y1 z1) + (VectorBox3d x2 y2 z2) =
        VectorBox3d (x1 + x2) (y1 + y2) (z1 + z2)

instance Subtraction (VectorBox3d (Qty a)) (VectorBox3d (Qty a)) (VectorBox3d (Qty a)) coordinates where
    (VectorBox3d x1 y1 z1) - (VectorBox3d x2 y2 z2) =
        VectorBox3d (x1 - x2) (y1 - y2) (z1 - z2)

instance Multiplication (Qty a) (Qty b) (Qty c) => Multiplication (Qty a) (VectorBox3d (Qty b) coordinates) (VectorBox3d (Qty c) coordinates) where
    value * (VectorBox3d x y z) =
        VectorBox3d (value * x) (value * y) (value * z)

instance Multiplication (Qty a) (Qty b) (Qty c) => Multiplication (VectorBox3d (Qty a) coordinates) (Qty b) (VectorBox3d (Qty c) coordinates) where
    (VectorBox3d x y z) * value =
        VectorBox3d (x * value) (y * value) (z * value)

instance Multiplication (Qty a) (Qty b) (Qty c) => Multiplication (Range (Qty a)) (VectorBox3d (Qty b) coordinates) (VectorBox3d (Qty c) coordinates) where
    range * (VectorBox3d x y z) =
        VectorBox3d (range * x) (range * y) (range * z)

instance Multiplication (Qty a) (Qty b) (Qty c) => Multiplication (VectorBox3d (Qty a) coordinates) (Range (Qty b)) (VectorBox3d (Qty c) coordinates) where
    (VectorBox3d x y z) * range =
        VectorBox3d (x * range) (y * range) (z * range)

instance Division (Qty a) (Qty b) (Qty c) => Division (VectorBox3d (Qty a) coordinates) (Qty b) (VectorBox3d (Qty c) coordinates) where
    (VectorBox3d x y z) / value =
        VectorBox3d (x / value) (y / value) (z / value)

instance Division (Qty a) (Qty b) (Qty c) => Division (VectorBox3d (Qty a) coordinates) (Range (Qty b)) (VectorBox3d (Qty c) coordinates) where
    (VectorBox3d x y z) / range =
        VectorBox3d (x / range) (y / range) (z / range)

instance Multiplication (Qty a) (Qty b) (Qty c) => DotProduct (Vector3d (Qty a)) (VectorBox3d (Qty b)) (Range (Qty c)) coordinates where
    (Vector3d x1 y1 z1) <> (VectorBox3d x2 y2 z2) =
        x1 * x2 + y1 * y2 + z1 * z2

instance Multiplication (Qty a) (Qty b) (Qty c) => DotProduct (VectorBox3d (Qty a)) (Vector3d (Qty b)) (Range (Qty c)) coordinates where
    (VectorBox3d x1 y1 z1) <> (Vector3d x2 y2 z2) =
        x1 * x2 + y1 * y2 + z1 * z2

instance Multiplication (Qty a) (Qty b) (Qty c) => DotProduct (VectorBox3d (Qty a)) (VectorBox3d (Qty b)) (Range (Qty c)) coordinates where
    (VectorBox3d x1 y1 z1) <> (VectorBox3d x2 y2 z2) =
        x1 * x2 + y1 * y2 + z1 * z2

instance Multiplication (Qty a) (Qty b) (Qty c) => CrossProduct (Vector3d (Qty a)) (VectorBox3d (Qty b)) (VectorBox3d (Qty c)) coordinates where
    (Vector3d x1 y1 z1) >< (VectorBox3d x2 y2 z2) =
        let x = y1 * z2 - z1 * y2
            y = z1 * x2 - x1 * z2
            z = x1 * y2 - y1 * x2
         in VectorBox3d x y z

instance Multiplication (Qty a) (Qty b) (Qty c) => CrossProduct (VectorBox3d (Qty a)) (Vector3d (Qty b)) (VectorBox3d (Qty c)) coordinates where
    (VectorBox3d x1 y1 z1) >< (Vector3d x2 y2 z2) =
        let x = y1 * z2 - z1 * y2
            y = z1 * x2 - x1 * z2
            z = x1 * y2 - y1 * x2
         in VectorBox3d x y z

instance Multiplication (Qty a) (Qty b) (Qty c) => CrossProduct (VectorBox3d (Qty a)) (VectorBox3d (Qty b)) (VectorBox3d (Qty c)) coordinates where
    (VectorBox3d x1 y1 z1) >< (VectorBox3d x2 y2 z2) =
        let x = y1 * z2 - z1 * y2
            y = z1 * x2 - x1 * z2
            z = x1 * y2 - y1 * x2
         in VectorBox3d x y z

constant :: Vector3d (Qty a) coordinates -> VectorBox3d (Qty a) coordinates
constant vector =
    let (Vector3d x y z) = vector
     in VectorBox3d (Range.constant x) (Range.constant y) (Range.constant z)

hull2 :: Vector3d (Qty a) coordinates -> Vector3d (Qty a) coordinates -> VectorBox3d (Qty a) coordinates
hull2 v1 v2 =
    let (Vector3d x1 y1 z1) = v1
        (Vector3d x2 y2 z2) = v2
     in VectorBox3d (Range.from x1 x2) (Range.from y1 y2) (Range.from z1 z2)

hull3 :: Vector3d (Qty a) coordinates -> Vector3d (Qty a) coordinates -> Vector3d (Qty a) coordinates -> VectorBox3d (Qty a) coordinates
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

hull4 :: Vector3d (Qty a) coordinates -> Vector3d (Qty a) coordinates -> Vector3d (Qty a) coordinates -> Vector3d (Qty a) coordinates -> VectorBox3d (Qty a) coordinates
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

squaredMagnitude :: Multiplication (Qty a) (Qty a) (Qty b) => VectorBox3d (Qty a) coordinates -> Range (Qty b)
squaredMagnitude vectorBox =
    let (VectorBox3d x y z) = vectorBox
     in Range.squared x + Range.squared y + Range.squared z

magnitude :: VectorBox3d (Qty a) coordinates -> Range (Qty a)
magnitude vectorBox =
    let (VectorBox3d x y z) = Units.drop vectorBox
     in Units.add (Range.sqrt (Range.squared x + Range.squared y + Range.squared z))

normalize :: VectorBox3d (Qty a) coordinates -> VectorBox3d Float coordinates
normalize vectorBox =
    let (VectorBox3d x y z) = vectorBox / magnitude vectorBox
        nx = clampNormalized x
        ny = clampNormalized y
        nz = clampNormalized z
     in VectorBox3d nx ny nz

clampNormalized :: Range Float -> Range Float
clampNormalized (Range low high) =
    Range (Float.clamp (-1.0) 1.0 low) (Float.clamp (-1.0) 1.0 high)
