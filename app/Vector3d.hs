module Vector3d (
    Vector3d (..),
    zero,
    x,
    y,
    z,
    xy,
    xz,
    yz,
    xyz,
    meters,
) where

import qualified Area
import Data.Fixed (Uni)
import Direction3d (Direction3d)
import qualified Direction3d
import Direction3d.Unsafe
import qualified Length
import qualified Length as Vector3d
import OpenSolid
import Quantity (Quantity)
import qualified Quantity
import qualified Show
import qualified String
import qualified Units

data Vector3d units coordinates = Vector3d (Quantity units) (Quantity units) (Quantity units)
    deriving (Eq)

instance Show (Vector3d Unitless coordinates) where
    showsPrec =
        showImpl "Vector3d" identity

instance Show (Vector3d Units.Meters coordinates) where
    showsPrec =
        showImpl "Vector3d.meters" Length.inMeters

instance Show (Vector3d Units.SquareMeters coordinates) where
    showsPrec =
        showImpl "Vector3d.squareMeters" Area.inSquareMeters

showImpl functionName inCorrespondingUnits precedence (Vector3d x y z) =
    Show.primitive precedence functionName [inCorrespondingUnits x, inCorrespondingUnits y, inCorrespondingUnits z]

instance Negation (Vector3d units coordinates) where
    negate (Vector3d x y z) =
        Vector3d (- x) (- y) (- z)

instance Addition (Vector3d units) (Vector3d units) where
    type Sum (Vector3d units) (Vector3d units) = Vector3d units
    (Vector3d x1 y1 z1) + (Vector3d x2 y2 z2) =
        Vector3d (x1 + x2) (y1 + y2) (z1 + z2)

instance Subtraction (Vector3d units) (Vector3d units) where
    type Difference (Vector3d units) (Vector3d units) = Vector3d units
    (Vector3d x1 y1 z1) - (Vector3d x2 y2 z2) =
        Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance Units.Multiplication units1 units2 => Multiplication (Quantity units1) (Vector3d units2 coordinates) where
    type Product (Quantity units1) (Vector3d units2 coordinates) = Vector3d (Units.Product units1 units2) coordinates
    scale * (Vector3d x y z) =
        Vector3d (scale * x) (scale * y) (scale * z)

instance Units.Multiplication units1 units2 => Multiplication (Vector3d units1 coordinates) (Quantity units2) where
    type Product (Vector3d units1 coordinates) (Quantity units2) = Vector3d (Units.Product units1 units2) coordinates
    (Vector3d x y z) * scale =
        Vector3d (x * scale) (y * scale) (z * scale)

instance Units.Division units1 units2 => Division (Vector3d units1 coordinates) (Quantity units2) where
    type Quotient (Vector3d units1 coordinates) (Quantity units2) = Vector3d (Units.Quotient units1 units2) coordinates
    (Vector3d x y z) / scale =
        Vector3d (x / scale) (y / scale) (z / scale)

instance Units.Multiplication units1 units2 => DotProduct (Vector3d units1) (Vector3d units2) where
    type DotProductResult (Vector3d units1) (Vector3d units2) = Quantity (Units.Product units1 units2)
    (Vector3d x1 y1 z1) . (Vector3d x2 y2 z2) =
        x1 * x2 + y1 * y2 + z1 * z2

instance DotProduct (Vector3d units) Direction3d where
    type DotProductResult (Vector3d units) Direction3d = Quantity units
    (Vector3d vx vy vz) . (Direction3d dx dy dz) =
        vx * dx + vy * dy + vz * dz

instance DotProduct Direction3d (Vector3d units) where
    type DotProductResult Direction3d (Vector3d units) = Quantity units
    (Direction3d dx dy dz) . (Vector3d vx vy vz) =
        dx * vx + dy * vy + dz * vz

instance Units.Multiplication units1 units2 => CrossProduct (Vector3d units1) (Vector3d units2) where
    type CrossProductResult (Vector3d units1) (Vector3d units2) = Vector3d (Units.Product units1 units2)
    (Vector3d x1 y1 z1) >< (Vector3d x2 y2 z2) =
        let x = y1 * z2 - z1 * y2
            y = z1 * x2 - x1 * z2
            z = x1 * y2 - y1 * x2
         in Vector3d x y z

instance Multiplication (Quantity units) (Direction3d coordinates) where
    type Product (Quantity units) (Direction3d coordinates) = Vector3d units coordinates
    scale * (Direction3d x y z) =
        Vector3d (scale * x) (scale * y) (scale * z)

instance Multiplication (Direction3d coordinates) (Quantity units) where
    type Product (Direction3d coordinates) (Quantity units) = Vector3d units coordinates
    (Direction3d x y z) * scale =
        Vector3d (x * scale) (y * scale) (z * scale)

instance CrossProduct (Vector3d units) Direction3d where
    type CrossProductResult (Vector3d units) Direction3d = Vector3d units
    (Vector3d vx vy vz) >< (Direction3d dx dy dz) =
        let x = vy * dz - vz * dy
            y = vz * dx - vx * dz
            z = vx * dy - vy * dx
         in Vector3d x y z

instance CrossProduct Direction3d (Vector3d units) where
    type CrossProductResult Direction3d (Vector3d units) = Vector3d units
    (Direction3d dx dy dz) >< (Vector3d vx vy vz) =
        let x = dy * vz - dz * vy
            y = dz * vx - dx * vz
            z = dx * vy - dy * vx
         in Vector3d x y z

instance CrossProduct Direction3d Direction3d where
    type CrossProductResult Direction3d Direction3d = Vector3d Unitless
    (Direction3d x1 y1 z1) >< (Direction3d x2 y2 z2) =
        Vector3d
            (y1 * z2 - z1 * y2)
            (z1 * x2 - x1 * z2)
            (x1 * y2 - y1 * x2)

zero :: Vector3d units coordinates
zero =
    Vector3d Quantity.zero Quantity.zero Quantity.zero

x :: Quantity units -> Vector3d units coordinates
x x =
    Vector3d x Quantity.zero Quantity.zero

y :: Quantity units -> Vector3d units coordinates
y y =
    Vector3d Quantity.zero y Quantity.zero

z :: Quantity units -> Vector3d units coordinates
z z =
    Vector3d Quantity.zero Quantity.zero z

xy :: Quantity units -> Quantity units -> Vector3d units coordinates
xy x y =
    Vector3d x y Quantity.zero

xz :: Quantity units -> Quantity units -> Vector3d units coordinates
xz x z =
    Vector3d x Quantity.zero z

yz :: Quantity units -> Quantity units -> Vector3d units coordinates
yz y z =
    Vector3d Quantity.zero y z

xyz :: Quantity units -> Quantity units -> Quantity units -> Vector3d units coordinates
xyz =
    Vector3d

meters :: Float -> Float -> Float -> Vector3d Units.Meters coordinates
meters x y z =
    Vector3d (Length.meters x) (Length.meters y) (Length.meters z)

squareMeters :: Float -> Float -> Float -> Vector3d Units.SquareMeters coordinates
squareMeters x y z =
    Vector3d (Area.squareMeters x) (Area.squareMeters y) (Area.squareMeters z)
