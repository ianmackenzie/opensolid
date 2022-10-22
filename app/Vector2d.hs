module Vector2d (
    Vector2d (..),
    zero,
    x,
    y,
    xy,
    meters,
    squareMeters,
    determinant,
    magnitude,
    squaredMagnitude,
    direction,
    normalize,
) where

import qualified Area
import Data.Coerce (coerce)
import Direction2d.Unsafe
import qualified Length
import OpenSolid
import qualified Quantity
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

instance Addition (Vector2d units) (Vector2d units) where
    type Sum (Vector2d units) (Vector2d units) = Vector2d units
    (Vector2d x1 y1) + (Vector2d x2 y2) =
        Vector2d (x1 + x2) (y1 + y2)

instance Subtraction (Vector2d units) (Vector2d units) where
    type Difference (Vector2d units) (Vector2d units) = Vector2d units
    (Vector2d x1 y1) - (Vector2d x2 y2) =
        Vector2d (x1 - x2) (y1 - y2)

instance Multiplication (Quantity units) (Direction2d coordinates) where
    type Product (Quantity units) (Direction2d coordinates) = Vector2d units coordinates
    scale * (Direction2d x y) =
        Vector2d (scale * x) (scale * y)

instance Multiplication (Direction2d coordinates) (Quantity units) where
    type Product (Direction2d coordinates) (Quantity units) = Vector2d units coordinates
    (Direction2d x y) * scale =
        Vector2d (x * scale) (y * scale)

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

zero :: Vector2d units coordinates
zero =
    Vector2d Quantity.zero Quantity.zero

x :: Quantity units -> Vector2d units coordinates
x vx =
    Vector2d vx Quantity.zero

y :: Quantity units -> Vector2d units coordinates
y vy =
    Vector2d Quantity.zero vy

xy :: Quantity units -> Quantity units -> Vector2d units coordinates
xy =
    Vector2d

meters :: Float -> Float -> Vector2d Units.Meters coordinates
meters vx vy =
    Vector2d (Length.meters vx) (Length.meters vy)

squareMeters :: Float -> Float -> Vector2d Units.SquareMeters coordinates
squareMeters vx vy =
    Vector2d (Area.squareMeters vx) (Area.squareMeters vy)

determinant ::
    Units.Multiplication units1 units2 =>
    Vector2d units1 coordinates ->
    Vector2d units2 coordinates ->
    Quantity (Units.Product units1 units2)
determinant (Vector2d x1 y1) (Vector2d x2 y2) =
    x1 * y2 - y1 * x2

magnitude :: Vector2d units coordinates -> Quantity units
magnitude vector =
    let (Vector2d vx vy) = coerce vector :: Vector2d Unitless coordinates
     in coerce (sqrt (vx * vx + vy * vy))

squaredMagnitude :: Units.Multiplication units units => Vector2d units coordinates -> Quantity (Units.Product units units)
squaredMagnitude vector =
    let (Vector2d vx vy) = vector in vx * vx + vy * vy

direction :: Vector2d units coordinates -> Maybe (Direction2d coordinates)
direction vector =
    let m = magnitude vector
     in if m == Quantity.zero
            then Nothing
            else
                let (Vector2d vx vy) = vector
                 in Just (Direction2d (vx / m) (vy / m))

normalize :: Vector2d units coordinates -> Vector2d Unitless coordinates
normalize vector =
    let m = magnitude vector
     in if m == Quantity.zero
            then zero
            else
                let (Vector2d vx vy) = vector
                 in Vector2d (vx / m) (vy / m)
