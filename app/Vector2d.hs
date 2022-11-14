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
import qualified Float
import qualified Length
import OpenSolid
import Point2d.Type
import qualified Qty
import qualified Units
import Vector2d.Type

zero :: Vector2d (Qty a) coordinates
zero =
    Vector2d Qty.zero Qty.zero

x :: Qty a -> Vector2d (Qty a) coordinates
x vx =
    Vector2d vx Qty.zero

y :: Qty a -> Vector2d (Qty a) coordinates
y vy =
    Vector2d Qty.zero vy

xy :: Qty a -> Qty a -> Vector2d (Qty a) coordinates
xy =
    Vector2d

meters :: Float -> Float -> Vector2d Length coordinates
meters vx vy =
    Vector2d (Length.meters vx) (Length.meters vy)

squareMeters :: Float -> Float -> Vector2d Area coordinates
squareMeters vx vy =
    Vector2d (Area.squareMeters vx) (Area.squareMeters vy)

from :: Point2d coordinates -> Point2d coordinates -> Vector2d Length coordinates
from p1 p2 =
    let (Point2d x1 y1) = p1
        (Point2d x2 y2) = p2
     in Vector2d (x2 - x1) (y2 - y1)

interpolateFrom :: Vector2d (Qty a) coordinates -> Vector2d (Qty a) coordinates -> Float -> Vector2d (Qty a) coordinates
interpolateFrom v1 v2 t =
    let (Vector2d x1 y1) = v1
        (Vector2d x2 y2) = v2
        vx = Qty.interpolateFrom x1 x2 t
        vy = Qty.interpolateFrom y1 y2 t
     in Vector2d vx vy

midpoint :: Vector2d (Qty a) coordinates -> Vector2d (Qty a) coordinates -> Vector2d (Qty a) coordinates
midpoint v1 v2 =
    let (Vector2d x1 y1) = v1
        (Vector2d x2 y2) = v2
        vx = Qty.midpoint x1 x2
        vy = Qty.midpoint y1 y2
     in Vector2d vx vy

determinant ::
    Multiplication (Qty a) (Qty b) (Qty c) =>
    Vector2d (Qty a) coordinates ->
    Vector2d (Qty b) coordinates ->
    Qty c
determinant (Vector2d x1 y1) (Vector2d x2 y2) =
    x1 * y2 - y1 * x2

magnitude :: Vector2d (Qty a) coordinates -> Qty a
magnitude vector =
    let (Vector2d vx vy) = vector
        fx = Units.drop vx
        fy = Units.drop vy
     in Units.add (Float.sqrt (fx * fx + fy * fy))

squaredMagnitude :: Multiplication (Qty a) (Qty a) (Qty b) => Vector2d (Qty a) coordinates -> Qty b
squaredMagnitude vector =
    let (Vector2d vx vy) = vector in vx * vx + vy * vy

direction :: Vector2d (Qty a) coordinates -> Maybe (Direction2d coordinates)
direction vector =
    let m = magnitude vector
     in if m == Qty.zero
            then Nothing
            else
                let (Vector2d vx vy) = vector
                 in Just (Direction2d (vx / m) (vy / m))

normalize :: Vector2d (Qty a) coordinates -> Vector2d Float coordinates
normalize vector =
    let m = magnitude vector
     in if m == Qty.zero
            then zero
            else
                let (Vector2d vx vy) = vector
                 in Vector2d (vx / m) (vy / m)
