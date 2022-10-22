module Vector2d (
    Vector2d,
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
import qualified Units
import Vector2d.Type

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
