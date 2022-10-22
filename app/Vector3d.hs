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
    squareMeters,
    midpoint,
    interpolateFrom,
    magnitude,
    squaredMagnitude,
    direction,
    normalize,
) where

import qualified Area
import Data.Coerce (coerce)
import Direction3d.Unsafe
import qualified Length
import OpenSolid
import qualified Quantity
import qualified Show
import qualified Units
import qualified Prelude

data Vector3d units coordinates = Vector3d !(Quantity units) !(Quantity units) !(Quantity units)
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

showImpl :: String -> (Quantity units -> Float) -> Prelude.Int -> Vector3d units coordinates -> Prelude.ShowS
showImpl functionName inCorrespondingUnits precedence (Vector3d vx vy vz) =
    let arguments = [inCorrespondingUnits vx, inCorrespondingUnits vy, inCorrespondingUnits vz]
     in Show.primitive precedence functionName arguments

instance Negation (Vector3d units coordinates) where
    negate (Vector3d vx vy vz) =
        Vector3d (negate vx) (negate vy) (negate vz)

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
    scale * (Vector3d vx vy vz) =
        Vector3d (scale * vx) (scale * vy) (scale * vz)

instance Units.Multiplication units1 units2 => Multiplication (Vector3d units1 coordinates) (Quantity units2) where
    type Product (Vector3d units1 coordinates) (Quantity units2) = Vector3d (Units.Product units1 units2) coordinates
    (Vector3d vx vy vz) * scale =
        Vector3d (vx * scale) (vy * scale) (vz * scale)

instance Units.Division units1 units2 => Division (Vector3d units1 coordinates) (Quantity units2) where
    type Quotient (Vector3d units1 coordinates) (Quantity units2) = Vector3d (Units.Quotient units1 units2) coordinates
    (Vector3d vx vy vz) / scale =
        Vector3d (vx / scale) (vy / scale) (vz / scale)

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
        let vx = y1 * z2 - z1 * y2
            vy = z1 * x2 - x1 * z2
            vz = x1 * y2 - y1 * x2
         in Vector3d vx vy vz

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
    (Vector3d x1 y1 z1) >< (Direction3d x2 y2 z2) =
        let vx = y1 * z2 - z1 * y2
            vy = z1 * x2 - x1 * z2
            vz = x1 * y2 - y1 * x2
         in Vector3d vx vy vz

instance CrossProduct Direction3d (Vector3d units) where
    type CrossProductResult Direction3d (Vector3d units) = Vector3d units
    (Direction3d x1 y1 z1) >< (Vector3d x2 y2 z2) =
        let vx = y1 * z2 - z1 * y2
            vy = z1 * x2 - x1 * z2
            vz = x1 * y2 - y1 * x2
         in Vector3d vx vy vz

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
x vx =
    Vector3d vx Quantity.zero Quantity.zero

y :: Quantity units -> Vector3d units coordinates
y vy =
    Vector3d Quantity.zero vy Quantity.zero

z :: Quantity units -> Vector3d units coordinates
z vz =
    Vector3d Quantity.zero Quantity.zero vz

xy :: Quantity units -> Quantity units -> Vector3d units coordinates
xy vx vz =
    Vector3d vx vz Quantity.zero

xz :: Quantity units -> Quantity units -> Vector3d units coordinates
xz vx vz =
    Vector3d vx Quantity.zero vz

yz :: Quantity units -> Quantity units -> Vector3d units coordinates
yz vy vz =
    Vector3d Quantity.zero vy vz

xyz :: Quantity units -> Quantity units -> Quantity units -> Vector3d units coordinates
xyz =
    Vector3d

meters :: Float -> Float -> Float -> Vector3d Units.Meters coordinates
meters vx vy vz =
    Vector3d (Length.meters vx) (Length.meters vy) (Length.meters vz)

squareMeters :: Float -> Float -> Float -> Vector3d Units.SquareMeters coordinates
squareMeters vx vy vz =
    Vector3d (Area.squareMeters vx) (Area.squareMeters vy) (Area.squareMeters vz)

interpolateFrom :: Vector3d units coordinates -> Vector3d units coordinates -> Float -> Vector3d units coordinates
interpolateFrom v1 v2 t =
    let (Vector3d x1 y1 z1) = v1
        (Vector3d x2 y2 z2) = v2
        vx = Quantity.interpolateFrom x1 x2 t
        vy = Quantity.interpolateFrom y1 y2 t
        vz = Quantity.interpolateFrom z1 z2 t
     in Vector3d vx vy vz

midpoint :: Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates
midpoint v1 v2 =
    let (Vector3d x1 y1 z1) = v1
        (Vector3d x2 y2 z2) = v2
        vx = Quantity.midpoint x1 x2
        vy = Quantity.midpoint y1 y2
        vz = Quantity.midpoint z1 z2
     in Vector3d vx vy vz

magnitude :: Vector3d units coordinates -> Quantity units
magnitude vector =
    let (Vector3d vx vy vz) = coerce vector :: Vector3d Unitless coordinates
     in coerce (sqrt (vx * vx + vy * vy + vz * vz))

squaredMagnitude :: Units.Multiplication units units => Vector3d units coordinates -> Quantity (Units.Product units units)
squaredMagnitude vector =
    let (Vector3d vx vy vz) = vector in vx * vx + vy * vy + vz * vz

direction :: Vector3d units coordinates -> Maybe (Direction3d coordinates)
direction vector =
    let m = magnitude vector
     in if m == Quantity.zero
            then Nothing
            else
                let (Vector3d vx vy vz) = vector
                 in Just (Direction3d (vx / m) (vy / m) (vz / m))

normalize :: Vector3d units coordinates -> Vector3d Unitless coordinates
normalize vector =
    let m = magnitude vector
     in if m == Quantity.zero
            then zero
            else
                let (Vector3d vx vy vz) = vector
                 in Vector3d (vx / m) (vy / m) (vz / m)
