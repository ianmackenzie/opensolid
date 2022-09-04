module Vector3d (
    Vector3d,
    zero,
    xyz,
    meters,
    components,
) where

import qualified Area
import Data.Fixed (Uni)
import Direction3d (Direction3d)
import qualified Direction3d
import qualified Length
import qualified Length as Vector3d
import OpenSolid
import Quantity (Quantity)
import qualified Quantity
import qualified String
import Units (Meters, SquareMeters)

newtype Vector3d units coordinates = Vector3d (Quantity units, Quantity units, Quantity units)
    deriving (Eq)

instance Show (Vector3d Unitless coordinates) where
    show =
        showImpl "Vector3d.xyz" identity

instance Show (Vector3d Meters coordinates) where
    show =
        showImpl "Vector3d.meters" Length.inMeters

instance Show (Vector3d SquareMeters coordinates) where
    show =
        showImpl "Vector3d.squareMeters" Area.inSquareMeters

showImpl functionName inCorrespondingUnits vector =
    let (x, y, z) = components vector
        xString = String.fromFloat (inCorrespondingUnits x)
        yString = String.fromFloat (inCorrespondingUnits y)
        zString = String.fromFloat (inCorrespondingUnits z)
     in String.toList (functionName ++ " " ++ xString ++ " " ++ yString ++ " " ++ zString)

instance Negation (Vector3d units coordinates) where
    negate (Vector3d (x, y, z)) =
        Vector3d (- x, - y, - z)

instance Addition (Vector3d units) (Vector3d units) where
    type Sum (Vector3d units) (Vector3d units) = Vector3d units
    (Vector3d (x1, y1, z1)) + (Vector3d (x2, y2, z2)) =
        Vector3d (x1 + x2, y1 + y2, z1 + z2)

instance Subtraction (Vector3d units) (Vector3d units) where
    type Difference (Vector3d units) (Vector3d units) = Vector3d units
    (Vector3d (x1, y1, z1)) - (Vector3d (x2, y2, z2)) =
        Vector3d (x1 - x2, y1 - y2, z1 - z2)

instance Multiplication units1 units2 => Multiplication (Quantity units1) (Vector3d units2 coordinates) where
    type Product (Quantity units1) (Vector3d units2 coordinates) = Vector3d (Product units1 units2) coordinates
    scale * (Vector3d (x, y, z)) =
        Vector3d (scale * x, scale * y, scale * z)

instance Multiplication units1 units2 => Multiplication (Vector3d units1 coordinates) (Quantity units2) where
    type Product (Vector3d units1 coordinates) (Quantity units2) = Vector3d (Product units1 units2) coordinates
    (Vector3d (x, y, z)) * scale =
        Vector3d (x * scale, y * scale, z * scale)

instance Division units1 units2 => Division (Vector3d units1 coordinates) (Quantity units2) where
    type Quotient (Vector3d units1 coordinates) (Quantity units2) = Vector3d (Quotient units1 units2) coordinates
    (Vector3d (x, y, z)) / scale =
        Vector3d (x / scale, y / scale, z / scale)

instance Multiplication units1 units2 => DotProduct (Vector3d units1) (Vector3d units2) where
    type DotProductResult (Vector3d units1) (Vector3d units2) = Quantity (Product units1 units2)
    (Vector3d (x1, y1, z1)) . (Vector3d (x2, y2, z2)) =
        x1 * x2 + y1 * y2 + z1 * z2

instance Multiplication units Unitless => DotProduct (Vector3d units) Direction3d where
    type DotProductResult (Vector3d units) Direction3d = Quantity (Product units Unitless)
    (Vector3d (vx, vy, vz)) . direction =
        let (dx, dy, dz) = Direction3d.components direction
         in vx * dx + vy * dy + vz * dz

instance Multiplication Unitless units => DotProduct Direction3d (Vector3d units) where
    type DotProductResult Direction3d (Vector3d units) = Quantity (Product Unitless units)
    direction . (Vector3d (vx, vy, vz)) =
        let (dx, dy, dz) = Direction3d.components direction
         in dx * vx + dy * vy + dz * vz

instance Multiplication units1 units2 => CrossProduct (Vector3d units1) (Vector3d units2) where
    type CrossProductResult (Vector3d units1) (Vector3d units2) = Vector3d (Product units1 units2)
    (Vector3d (x1, y1, z1)) >< (Vector3d (x2, y2, z2)) =
        let x = y1 * z2 - z1 * y2
            y = z1 * x2 - x1 * z2
            z = x1 * y2 - y1 * x2
         in Vector3d (x, y, z)

instance Multiplication units Unitless => CrossProduct (Vector3d units) Direction3d where
    type CrossProductResult (Vector3d units) Direction3d = Vector3d (Product units Unitless)
    (Vector3d (vx, vy, vz)) >< direction =
        let (dx, dy, dz) = Direction3d.components direction
            x = vy * dz - vz * dy
            y = vz * dx - vx * dz
            z = vx * dy - vy * dx
         in Vector3d (x, y, z)

instance Multiplication Unitless units => CrossProduct Direction3d (Vector3d units) where
    type CrossProductResult Direction3d (Vector3d units) = Vector3d (Product Unitless units)
    direction >< (Vector3d (vx, vy, vz)) =
        let (dx, dy, dz) = Direction3d.components direction
            x = dy * vz - dz * vy
            y = dz * vx - dx * vz
            z = dx * vy - dy * vx
         in Vector3d (x, y, z)

instance Zero (Vector3d units coordinates) where
    zero =
        Vector3d (zero, zero, zero)

xyz :: Quantity units -> Quantity units -> Quantity units -> Vector3d units coordinates
xyz x y z =
    Vector3d (x, y, z)

meters :: Float -> Float -> Float -> Vector3d Meters coordinates
meters x y z =
    Vector3d (Length.meters x, Length.meters y, Length.meters z)

squareMeters :: Float -> Float -> Float -> Vector3d SquareMeters coordinates
squareMeters x y z =
    Vector3d (Area.squareMeters x, Area.squareMeters y, Area.squareMeters z)

components :: Vector3d units coordinates -> (Quantity units, Quantity units, Quantity units)
components (Vector3d components) =
    components
