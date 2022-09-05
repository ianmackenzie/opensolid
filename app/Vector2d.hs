module Vector2d (
    Vector2d (..),
    zero,
    meters,
    determinant,
) where

import qualified Area
import Direction2d (Direction2d)
import Direction2d.Unsafe
import qualified Length
import OpenSolid
import Quantity (Quantity)
import qualified Quantity
import qualified String
import qualified Units

data Vector2d units coordinates = Vector2d (Quantity units) (Quantity units)
    deriving (Eq)

instance Show (Vector2d Unitless coordinates) where
    show =
        showImpl "Vector2d" identity

instance Show (Vector2d Units.Meters coordinates) where
    show =
        showImpl "Vector2d.meters" Length.inMeters

instance Show (Vector2d Units.SquareMeters coordinates) where
    show =
        showImpl "Vector2d.squareMeters" Area.inSquareMeters

showImpl functionName inCorrespondingUnits (Vector2d x y) =
    let xString = String.fromFloat (inCorrespondingUnits x)
        yString = String.fromFloat (inCorrespondingUnits y)
     in String.toList (functionName ++ " " ++ xString ++ " " ++ yString)

instance Negation (Vector2d units coordinates) where
    negate (Vector2d x y) =
        Vector2d (- x) (- y)

instance Addition (Vector2d units) (Vector2d units) where
    type Sum (Vector2d units) (Vector2d units) = Vector2d units
    (Vector2d x1 y1) + (Vector2d x2 y2) =
        Vector2d (x1 + x2) (y1 + y2)

instance Subtraction (Vector2d units) (Vector2d units) where
    type Difference (Vector2d units) (Vector2d units) = Vector2d units
    (Vector2d x1 y1) - (Vector2d x2 y2) =
        Vector2d (x1 - x2) (y1 - y2)

instance Units.Multiplication units Unitless => Multiplication (Quantity units) (Direction2d coordinates) where
    type Product (Quantity units) (Direction2d coordinates) = Vector2d (Units.Product units Unitless) coordinates
    scale * (Direction2d x y) =
        Vector2d (scale * x) (scale * y)

instance Units.Multiplication units1 units2 => Multiplication (Quantity units1) (Vector2d units2 coordinates) where
    type Product (Quantity units1) (Vector2d units2 coordinates) = Vector2d (Units.Product units1 units2) coordinates
    scale * (Vector2d x y) =
        Vector2d (scale * x) (scale * y)

instance Units.Multiplication units1 units2 => Multiplication (Vector2d units1 coordinates) (Quantity units2) where
    type Product (Vector2d units1 coordinates) (Quantity units2) = Vector2d (Units.Product units1 units2) coordinates
    (Vector2d x y) * scale =
        Vector2d (x * scale) (y * scale)

instance Units.Division units1 units2 => Division (Vector2d units1 coordinates) (Quantity units2) where
    type Quotient (Vector2d units1 coordinates) (Quantity units2) = Vector2d (Units.Quotient units1 units2) coordinates
    (Vector2d x y) / scale = Vector2d (x / scale) (y / scale)

instance Units.Multiplication units1 units2 => DotProduct (Vector2d units1) (Vector2d units2) where
    type DotProductResult (Vector2d units1) (Vector2d units2) = Quantity (Units.Product units1 units2)
    (Vector2d x1 y1) . (Vector2d x2 y2) =
        x1 * x2 + y1 * y2

instance Zero (Vector2d units coordinates) where
    zero =
        Vector2d zero zero

meters :: Float -> Float -> Vector2d Units.Meters coordinates
meters x y =
    Vector2d (Length.meters x) (Length.meters y)

squareMeters :: Float -> Float -> Vector2d Units.SquareMeters coordinates
squareMeters x y =
    Vector2d (Area.squareMeters x) (Area.squareMeters y)

determinant ::
    Units.Multiplication units1 units2 =>
    Vector2d units1 coordinates ->
    Vector2d units2 coordinates ->
    Quantity (Units.Product units1 units2)
determinant (Vector2d x1 y1) (Vector2d x2 y2) =
    x1 * y2 - y1 * x2
