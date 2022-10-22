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

import Data.Coerce (coerce)
import Interval (Interval)
import qualified Interval
import OpenSolid
import qualified Quantity
import qualified Range
import Range.Unsafe
import qualified Units
import Vector3d (Vector3d (..))

data VectorBox3d units coordinates = VectorBox3d !(Range units) !(Range units) !(Range units)

instance Negation (VectorBox3d units coordinates) where
    negate (VectorBox3d x y z) =
        VectorBox3d (negate x) (negate y) (negate z)

instance Addition (VectorBox3d units) (Vector3d units) where
    type Sum (VectorBox3d units) (Vector3d units) = VectorBox3d units
    (VectorBox3d x1 y1 z1) + (Vector3d x2 y2 z2) =
        VectorBox3d (x1 + x2) (y1 + y2) (z1 + z2)

instance Addition (Vector3d units) (VectorBox3d units) where
    type Sum (Vector3d units) (VectorBox3d units) = VectorBox3d units
    (Vector3d x1 y1 z1) + (VectorBox3d x2 y2 z2) =
        VectorBox3d (x1 + x2) (y1 + y2) (z1 + z2)

instance Addition (VectorBox3d units) (VectorBox3d units) where
    type Sum (VectorBox3d units) (VectorBox3d units) = VectorBox3d units
    (VectorBox3d x1 y1 z1) + (VectorBox3d x2 y2 z2) =
        VectorBox3d (x1 + x2) (y1 + y2) (z1 + z2)

instance Subtraction (VectorBox3d units) (Vector3d units) where
    type Difference (VectorBox3d units) (Vector3d units) = VectorBox3d units
    (VectorBox3d x1 y1 z1) - (Vector3d x2 y2 z2) =
        VectorBox3d (x1 - x2) (y1 - y2) (z1 - z2)

instance Subtraction (Vector3d units) (VectorBox3d units) where
    type Difference (Vector3d units) (VectorBox3d units) = VectorBox3d units
    (Vector3d x1 y1 z1) - (VectorBox3d x2 y2 z2) =
        VectorBox3d (x1 - x2) (y1 - y2) (z1 - z2)

instance Subtraction (VectorBox3d units) (VectorBox3d units) where
    type Difference (VectorBox3d units) (VectorBox3d units) = VectorBox3d units
    (VectorBox3d x1 y1 z1) - (VectorBox3d x2 y2 z2) =
        VectorBox3d (x1 - x2) (y1 - y2) (z1 - z2)

instance Units.Multiplication units1 units2 => Multiplication (Quantity units1) (VectorBox3d units2 coordinates) where
    type Product (Quantity units1) (VectorBox3d units2 coordinates) = VectorBox3d (Units.Product units1 units2) coordinates
    quantity * (VectorBox3d x y z) =
        VectorBox3d (quantity * x) (quantity * y) (quantity * z)

instance Units.Multiplication units1 units2 => Multiplication (VectorBox3d units1 coordinates) (Quantity units2) where
    type Product (VectorBox3d units1 coordinates) (Quantity units2) = VectorBox3d (Units.Product units1 units2) coordinates
    (VectorBox3d x y z) * quantity =
        VectorBox3d (x * quantity) (y * quantity) (z * quantity)

instance Units.Multiplication units1 units2 => Multiplication (Range units1) (VectorBox3d units2 coordinates) where
    type Product (Range units1) (VectorBox3d units2 coordinates) = VectorBox3d (Units.Product units1 units2) coordinates
    range * (VectorBox3d x y z) =
        VectorBox3d (range * x) (range * y) (range * z)

instance Units.Multiplication units1 units2 => Multiplication (VectorBox3d units1 coordinates) (Range units2) where
    type Product (VectorBox3d units1 coordinates) (Range units2) = VectorBox3d (Units.Product units1 units2) coordinates
    (VectorBox3d x y z) * range =
        VectorBox3d (x * range) (y * range) (z * range)

instance Units.Division units1 units2 => Division (VectorBox3d units1 coordinates) (Quantity units2) where
    type Quotient (VectorBox3d units1 coordinates) (Quantity units2) = VectorBox3d (Units.Quotient units1 units2) coordinates
    (VectorBox3d x y z) / quantity =
        VectorBox3d (x / quantity) (y / quantity) (z / quantity)

instance Units.Division units1 units2 => Division (VectorBox3d units1 coordinates) (Range units2) where
    type Quotient (VectorBox3d units1 coordinates) (Range units2) = VectorBox3d (Units.Quotient units1 units2) coordinates
    (VectorBox3d x y z) / range =
        VectorBox3d (x / range) (y / range) (z / range)

instance Units.Multiplication units1 units2 => DotProduct (VectorBox3d units1) (Vector3d units2) where
    type DotProductResult (VectorBox3d units1) (Vector3d units2) = Range (Units.Product units1 units2)
    (VectorBox3d x1 y1 z1) . (Vector3d x2 y2 z2) =
        x1 * x2 + y1 * y2 + z1 * z2

instance Units.Multiplication units1 units2 => DotProduct (Vector3d units1) (VectorBox3d units2) where
    type DotProductResult (Vector3d units1) (VectorBox3d units2) = Range (Units.Product units1 units2)
    (Vector3d x1 y1 z1) . (VectorBox3d x2 y2 z2) =
        x1 * x2 + y1 * y2 + z1 * z2

instance Units.Multiplication units1 units2 => DotProduct (VectorBox3d units1) (VectorBox3d units2) where
    type DotProductResult (VectorBox3d units1) (VectorBox3d units2) = Range (Units.Product units1 units2)
    (VectorBox3d x1 y1 z1) . (VectorBox3d x2 y2 z2) =
        x1 * x2 + y1 * y2 + z1 * z2

instance Units.Multiplication units1 units2 => CrossProduct (VectorBox3d units1) (Vector3d units2) where
    type CrossProductResult (VectorBox3d units1) (Vector3d units2) = VectorBox3d (Units.Product units1 units2)
    (VectorBox3d x1 y1 z1) >< (Vector3d x2 y2 z2) =
        let x = y1 * z2 - z1 * y2
            y = z1 * x2 - x1 * z2
            z = x1 * y2 - y1 * x2
         in VectorBox3d x y z

instance Units.Multiplication units1 units2 => CrossProduct (Vector3d units1) (VectorBox3d units2) where
    type CrossProductResult (Vector3d units1) (VectorBox3d units2) = VectorBox3d (Units.Product units1 units2)
    (Vector3d x1 y1 z1) >< (VectorBox3d x2 y2 z2) =
        let x = y1 * z2 - z1 * y2
            y = z1 * x2 - x1 * z2
            z = x1 * y2 - y1 * x2
         in VectorBox3d x y z

instance Units.Multiplication units1 units2 => CrossProduct (VectorBox3d units1) (VectorBox3d units2) where
    type CrossProductResult (VectorBox3d units1) (VectorBox3d units2) = VectorBox3d (Units.Product units1 units2)
    (VectorBox3d x1 y1 z1) >< (VectorBox3d x2 y2 z2) =
        let x = y1 * z2 - z1 * y2
            y = z1 * x2 - x1 * z2
            z = x1 * y2 - y1 * x2
         in VectorBox3d x y z

constant :: Vector3d units coordinates -> VectorBox3d units coordinates
constant vector =
    let (Vector3d x y z) = vector
     in VectorBox3d (Range.constant x) (Range.constant y) (Range.constant z)

hull2 :: Vector3d units coordinates -> Vector3d units coordinates -> VectorBox3d units coordinates
hull2 v1 v2 =
    let (Vector3d x1 y1 z1) = v1
        (Vector3d x2 y2 z2) = v2
     in VectorBox3d (Range.from x1 x2) (Range.from y1 y2) (Range.from z1 z2)

hull3 :: Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates -> VectorBox3d units coordinates
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

hull4 :: Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates -> VectorBox3d units coordinates
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

squaredMagnitude :: Units.Multiplication units units => VectorBox3d units coordinates -> Range (Units.Product units units)
squaredMagnitude vectorBox =
    let (VectorBox3d x y z) = vectorBox
     in Range.squared x + Range.squared y + Range.squared z

magnitude :: VectorBox3d units coordinates -> Range units
magnitude vectorBox =
    let (VectorBox3d x y z) = coerce vectorBox :: VectorBox3d Unitless coordinates
     in coerce (Interval.sqrt (Interval.squared x + Interval.squared y + Interval.squared z))

normalize :: VectorBox3d units coordinates -> VectorBox3d Unitless coordinates
normalize vectorBox =
    let (VectorBox3d x y z) = vectorBox / magnitude vectorBox
        nx = clampNormalized x
        ny = clampNormalized y
        nz = clampNormalized z
     in VectorBox3d nx ny nz

clampNormalized :: Interval -> Interval
clampNormalized (Range low high) =
    Range (Quantity.clamp (-1.0) 1.0 low) (Quantity.clamp (-1.0) 1.0 high)
