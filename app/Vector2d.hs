module Vector2d (
    Vector2d,
    zero,
    xy,
    meters,
    components,
    dotProduct,
    crossProduct,
) where

import qualified Length
import OpenSolid
import Quantity (Quantity)
import qualified Quantity
import qualified String
import Units (Meters)

newtype Vector2d units coordinates = Vector2d (Quantity units, Quantity units)
    deriving (Eq)

instance Show (Vector2d Meters coordinates) where
    show vector =
        let (x, y) = components vector
            xString = String.fromFloat (Length.inMeters x)
            yString = String.fromFloat (Length.inMeters y)
         in String.toList ("Vector2d.meters " ++ xString ++ " " ++ yString)

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

instance Multiplication (Quantity units1) (Quantity units2) (Quantity units3) => Multiplication (Quantity units1) (Vector2d units2 coordinates) (Vector2d units3 coordinates) where
    scale * (Vector2d (x, y)) = Vector2d (scale * x, scale * y)

instance Multiplication (Quantity units1) (Quantity units2) (Quantity units3) => Multiplication (Vector2d units1 coordinates) (Quantity units2) (Vector2d units3 coordinates) where
    (Vector2d (x, y)) * scale = Vector2d (x * scale, y * scale)

instance Division (Quantity units1) (Quantity units2) (Quantity units3) => Division (Vector2d units1 coordinates) (Quantity units2) (Vector2d units3 coordinates) where
    (Vector2d (x, y)) / scale = Vector2d (x / scale, y / scale)

zero :: Vector2d units coordinates
zero = Vector2d (Quantity.zero, Quantity.zero)

xy :: Quantity units -> Quantity units -> Vector2d units coordinates
xy x y =
    Vector2d (x, y)

meters :: Float -> Float -> Vector2d Meters coordinates
meters x y =
    Vector2d (Length.meters x, Length.meters y)

components :: Vector2d units coordinates -> (Quantity units, Quantity units)
components (Vector2d components) = components

dotProduct ::
    Multiplication (Quantity units1) (Quantity units2) (Quantity units3) =>
    Vector2d units1 coordinates ->
    Vector2d units2 coordinates ->
    Quantity units3
dotProduct v1 v2 =
    let (x1, y1) = components v1
        (x2, y2) = components v2
     in x1 * x2 + y1 * y2

crossProduct ::
    Multiplication (Quantity units1) (Quantity units2) (Quantity units3) =>
    Vector2d units1 coordinates ->
    Vector2d units2 coordinates ->
    Quantity units3
crossProduct v1 v2 =
    let (x1, y1) = components v1
        (x2, y2) = components v2
     in x1 * y2 - y1 * x2
