module Vector3d.Type (Vector3d (..)) where

import qualified Area
import qualified Length
import OpenSolid
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

instance Units.Multiplication units1 units2 => CrossProduct (Vector3d units1) (Vector3d units2) where
    type CrossProductResult (Vector3d units1) (Vector3d units2) = Vector3d (Units.Product units1 units2)
    (Vector3d x1 y1 z1) >< (Vector3d x2 y2 z2) =
        let vx = y1 * z2 - z1 * y2
            vy = z1 * x2 - x1 * z2
            vz = x1 * y2 - y1 * x2
         in Vector3d vx vy vz
