module Vector2d.Type (Vector2d (..)) where

import qualified Area
import qualified Length
import OpenSolid
import qualified Show
import qualified Units
import qualified Prelude

data Vector2d units coordinates = Vector2d !(Quantity units) !(Quantity units)
    deriving (Eq)

instance Show (Vector2d Unitless coordinates) where
    showsPrec =
        showImpl "Vector2d" identity

instance Show (Vector2d Units.Meters coordinates) where
    showsPrec =
        showImpl "Vector2d.meters" Length.inMeters

instance Show (Vector2d Units.SquareMeters coordinates) where
    showsPrec =
        showImpl "Vector2d.squareMeters" Area.inSquareMeters

showImpl :: String -> (Quantity units -> Float) -> Prelude.Int -> Vector2d units coordinates -> Prelude.ShowS
showImpl functionName inCorrespondingUnits precedence (Vector2d xComponent yComponent) =
    Show.primitive precedence functionName [inCorrespondingUnits xComponent, inCorrespondingUnits yComponent]

instance Negation (Vector2d units coordinates) where
    negate (Vector2d vx vy) =
        Vector2d (negate vx) (negate vy)

instance Addition (Vector2d units coordinates) where
    (Vector2d x1 y1) + (Vector2d x2 y2) =
        Vector2d (x1 + x2) (y1 + y2)

instance Subtraction (Vector2d units coordinates) where
    (Vector2d x1 y1) - (Vector2d x2 y2) =
        Vector2d (x1 - x2) (y1 - y2)

instance Units.Multiplication units1 units2 => Multiplication (Quantity units1) (Vector2d units2 coordinates) where
    type Product (Quantity units1) (Vector2d units2 coordinates) = Vector2d (Units.Product units1 units2) coordinates
    scale * (Vector2d vx vy) =
        Vector2d (scale * vx) (scale * vy)

instance Units.Multiplication units1 units2 => Multiplication (Vector2d units1 coordinates) (Quantity units2) where
    type Product (Vector2d units1 coordinates) (Quantity units2) = Vector2d (Units.Product units1 units2) coordinates
    (Vector2d vx vy) * scale =
        Vector2d (vx * scale) (vy * scale)

instance Units.Division units1 units2 => Division (Vector2d units1 coordinates) (Quantity units2) where
    type Quotient (Vector2d units1 coordinates) (Quantity units2) = Vector2d (Units.Quotient units1 units2) coordinates
    (Vector2d vx vy) / scale =
        Vector2d (vx / scale) (vy / scale)

instance Units.Multiplication units1 units2 => DotProduct (Vector2d units1) (Vector2d units2) where
    type DotProductResult (Vector2d units1) (Vector2d units2) = Quantity (Units.Product units1 units2)
    (Vector2d x1 y1) . (Vector2d x2 y2) =
        x1 * x2 + y1 * y2
