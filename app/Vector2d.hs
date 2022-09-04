module Vector2d (
    Vector2d,
    zero,
    xy,
    meters,
    components,
    determinant,
) where

import qualified Area
import qualified Length
import OpenSolid
import Quantity (Quantity)
import qualified Quantity
import qualified String
import Units (Meters, SquareMeters)

newtype Vector2d units coordinates = Vector2d (Quantity units, Quantity units)
    deriving (Eq)

instance Show (Vector2d Unitless coordinates) where
    show =
        showImpl "Vector2d.xy" identity

instance Show (Vector2d Meters coordinates) where
    show =
        showImpl "Vector2d.meters" Length.inMeters

instance Show (Vector2d SquareMeters coordinates) where
    show =
        showImpl "Vector2d.squareMeters" Area.inSquareMeters

showImpl functionName inCorrespondingUnits vector =
    let (x, y) = components vector
        xString = String.fromFloat (inCorrespondingUnits x)
        yString = String.fromFloat (inCorrespondingUnits y)
     in String.toList (functionName ++ " " ++ xString ++ " " ++ yString)

instance Negation (Vector2d units coordinates) where
    negate (Vector2d (x, y)) =
        Vector2d (- x, - y)

instance Addition (Vector2d units) (Vector2d units) where
    type Sum (Vector2d units) (Vector2d units) = Vector2d units
    (Vector2d (x1, y1)) + (Vector2d (x2, y2)) =
        Vector2d (x1 + x2, y1 + y2)

instance Subtraction (Vector2d units) (Vector2d units) where
    type Difference (Vector2d units) (Vector2d units) = Vector2d units
    (Vector2d (x1, y1)) - (Vector2d (x2, y2)) =
        Vector2d (x1 - x2, y1 - y2)

instance Multiplication units1 units2 => Multiplication (Quantity units1) (Vector2d units2 coordinates) where
    type Product (Quantity units1) (Vector2d units2 coordinates) = Vector2d (Product units1 units2) coordinates
    scale * (Vector2d (x, y)) =
        Vector2d (scale * x, scale * y)

instance Multiplication units1 units2 => Multiplication (Vector2d units1 coordinates) (Quantity units2) where
    type Product (Vector2d units1 coordinates) (Quantity units2) = Vector2d (Product units1 units2) coordinates
    (Vector2d (x, y)) * scale =
        Vector2d (x * scale, y * scale)

instance Division units1 units2 => Division (Vector2d units1 coordinates) (Quantity units2) where
    type Quotient (Vector2d units1 coordinates) (Quantity units2) = Vector2d (Quotient units1 units2) coordinates
    (Vector2d (x, y)) / scale = Vector2d (x / scale, y / scale)

instance Multiplication units1 units2 => DotProduct (Vector2d units1) (Vector2d units2) where
    type DotProductResult (Vector2d units1) (Vector2d units2) = Quantity (Product units1 units2)
    (Vector2d (x1, y1)) . (Vector2d (x2, y2)) =
        x1 * x2 + y1 * y2

zero :: Vector2d units coordinates
zero = Vector2d (Quantity.zero, Quantity.zero)

xy :: Quantity units -> Quantity units -> Vector2d units coordinates
xy x y =
    Vector2d (x, y)

meters :: Float -> Float -> Vector2d Meters coordinates
meters x y =
    Vector2d (Length.meters x, Length.meters y)

squareMeters :: Float -> Float -> Vector2d SquareMeters coordinates
squareMeters x y =
    Vector2d (Area.squareMeters x, Area.squareMeters y)

components :: Vector2d units coordinates -> (Quantity units, Quantity units)
components (Vector2d components) =
    components

determinant ::
    Multiplication units1 units2 =>
    Vector2d units1 coordinates ->
    Vector2d units2 coordinates ->
    Quantity (Product units1 units2)
determinant v1 v2 =
    let (x1, y1) = components v1
        (x2, y2) = components v2
     in x1 * y2 - y1 * x2
