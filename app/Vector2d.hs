module Vector2d (
    Vector2d,
    zero,
    x,
    y,
    xy,
    meters,
    squareMeters,
    from,
    midpoint,
    interpolateFrom,
    determinant,
    magnitude,
    squaredMagnitude,
    direction,
    normalize,
) where

import qualified Area
import Direction2d.Unsafe
import qualified Length
import OpenSolid hiding (zero)
import Point2d.Type
import qualified Qty
import qualified Units
import Vector2d.Type

zero :: Vector2d units coordinates
zero = Vector2d Qty.zero Qty.zero

x :: Qty units -> Vector2d units coordinates
x vx = Vector2d vx Qty.zero

y :: Qty units -> Vector2d units coordinates
y vy = Vector2d Qty.zero vy

xy :: Qty units -> Qty units -> Vector2d units coordinates
xy = Vector2d

meters :: Float -> Float -> Vector2d Meters coordinates
meters vx vy = Vector2d (Length.meters vx) (Length.meters vy)

squareMeters :: Float -> Float -> Vector2d SquareMeters coordinates
squareMeters vx vy = Vector2d (Area.squareMeters vx) (Area.squareMeters vy)

from :: Point2d coordinates -> Point2d coordinates -> Vector2d Meters coordinates
from (Point2d x1 y1) (Point2d x2 y2) = Vector2d (x2 - x1) (y2 - y1)

interpolateFrom :: Vector2d units coordinates -> Vector2d units coordinates -> Float -> Vector2d units coordinates
interpolateFrom (Vector2d x1 y1) (Vector2d x2 y2) t =
    Vector2d (Qty.interpolateFrom x1 x2 t) (Qty.interpolateFrom y1 y2 t)

midpoint :: Vector2d units coordinates -> Vector2d units coordinates -> Vector2d units coordinates
midpoint (Vector2d x1 y1) (Vector2d x2 y2) =
    Vector2d (Qty.midpoint x1 x2) (Qty.midpoint y1 y2)

determinant ::
    Multiplication (Qty units1) (Qty units2) (Qty units3) =>
    Vector2d units1 coordinates ->
    Vector2d units2 coordinates ->
    Qty units3
determinant (Vector2d x1 y1) (Vector2d x2 y2) = x1 * y2 - y1 * x2

magnitude :: Vector2d units coordinates -> Qty units
magnitude (Vector2d vx vy) =
    let fx = Units.drop vx
        fy = Units.drop vy
     in Units.add (sqrt (fx * fx + fy * fy))

squaredMagnitude :: Multiplication (Qty units1) (Qty units1) (Qty units2) => Vector2d units1 coordinates -> Qty units2
squaredMagnitude (Vector2d vx vy) = vx * vx + vy * vy

direction :: Vector2d units coordinates -> Maybe (Direction2d coordinates)
direction vector =
    let m = magnitude vector
     in if m == Qty.zero
            then Nothing
            else
                let (Vector2d vx vy) = vector
                 in Just (Direction2d (vx / m) (vy / m))

normalize :: Vector2d units coordinates -> Vector2d Unitless coordinates
normalize vector =
    let m = magnitude vector
     in if m == Qty.zero
            then zero
            else
                let (Vector2d vx vy) = vector
                 in Vector2d (vx / m) (vy / m)
