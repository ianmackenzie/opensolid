module Vector2d (
    Vector2d,
    zero,
    xy,
    meters,
    dotProduct,
) where

import qualified Length
import OpenSolid
import Quantity (Quantity)
import qualified Quantity
import Units (Meters)

newtype Vector2d units coordinates = Vector2d (Quantity units, Quantity units)

instance Negation (Vector2d units coordinates) where
    negate (Vector2d (x, y)) = Vector2d (- x, - y)

instance Addition (Vector2d units coordinates) (Vector2d units coordinates) (Vector2d units coordinates) where
    (Vector2d (x1, y1)) + (Vector2d (x2, y2)) = Vector2d (x1 + x2, y1 + y2)

instance Subtraction (Vector2d units coordinates) (Vector2d units coordinates) (Vector2d units coordinates) where
    (Vector2d (x1, y1)) - (Vector2d (x2, y2)) = Vector2d (x1 - x2, y1 - y2)

instance Multiplication (Quantity lhsUnits) (Quantity rhsUnits) (Quantity resultUnits) => Multiplication (Quantity lhsUnits) (Vector2d rhsUnits coordinates) (Vector2d resultUnits coordinates) where
    scale * (Vector2d (x, y)) = Vector2d (scale * x, scale * y)

instance Multiplication (Quantity lhsUnits) (Quantity rhsUnits) (Quantity resultUnits) => Multiplication (Vector2d lhsUnits coordinates) (Quantity rhsUnits) (Vector2d resultUnits coordinates) where
    (Vector2d (x, y)) * scale = Vector2d (x * scale, y * scale)

instance Division (Quantity lhsUnits) (Quantity rhsUnits) (Quantity resultUnits) => Division (Vector2d lhsUnits coordinates) (Quantity rhsUnits) (Vector2d resultUnits coordinates) where
    (Vector2d (x, y)) / scale = Vector2d (x / scale, y / scale)

zero :: Vector2d units coordinates
zero = Vector2d (Quantity.zero, Quantity.zero)

xy :: Quantity units -> Quantity units -> Vector2d units coordinates
xy x y = Vector2d (x, y)

meters :: Float -> Float -> Vector2d Meters coordinates
meters x y = Vector2d (Length.meters x, Length.meters y)

components :: Vector2d units coordinates -> (Quantity units, Quantity units)
components (Vector2d components) = components

dotProduct ::
    Multiplication (Quantity lhsUnits) (Quantity rhsUnits) (Quantity resultUnits) =>
    Vector2d lhsUnits coordinates ->
    Vector2d rhsUnits coordinates ->
    Quantity resultUnits
dotProduct v1 v2 = x1 * x2 + y1 * y2
  where
    (x1, y1) = components v1
    (x2, y2) = components v2
