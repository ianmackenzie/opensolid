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
import qualified Scalar
import qualified Units
import Vector2d.Type

zero :: Scalar scalar => Vector2d scalar coordinates
zero =
    Vector2d Scalar.zero Scalar.zero

x :: Scalar scalar => scalar -> Vector2d scalar coordinates
x vx =
    Vector2d vx Scalar.zero

y :: Scalar scalar => scalar -> Vector2d scalar coordinates
y vy =
    Vector2d Scalar.zero vy

xy :: Scalar scalar => scalar -> scalar -> Vector2d scalar coordinates
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

interpolateFrom :: Scalar scalar => Vector2d scalar coordinates -> Vector2d scalar coordinates -> Float -> Vector2d scalar coordinates
interpolateFrom v1 v2 t =
    let (Vector2d x1 y1) = v1
        (Vector2d x2 y2) = v2
        vx = Scalar.interpolateFrom x1 x2 t
        vy = Scalar.interpolateFrom y1 y2 t
     in Vector2d vx vy

midpoint :: Scalar scalar => Vector2d scalar coordinates -> Vector2d scalar coordinates -> Vector2d scalar coordinates
midpoint v1 v2 =
    let (Vector2d x1 y1) = v1
        (Vector2d x2 y2) = v2
        vx = Scalar.midpoint x1 x2
        vy = Scalar.midpoint y1 y2
     in Vector2d vx vy

determinant ::
    (Scalar scalar1, Scalar scalar2, Scalar result, Multiplication scalar1 scalar2 result) =>
    Vector2d scalar1 coordinates ->
    Vector2d scalar2 coordinates ->
    result
determinant (Vector2d x1 y1) (Vector2d x2 y2) =
    x1 * y2 - y1 * x2

magnitude :: Scalar scalar => Vector2d scalar coordinates -> scalar
magnitude vector =
    let (Vector2d vx vy) = vector
        fx = Units.drop vx
        fy = Units.drop vy
     in Units.add (Float.sqrt (fx * fx + fy * fy))

squaredMagnitude :: (Scalar scalar, Scalar squaredScalar, Multiplication scalar scalar squaredScalar) => Vector2d scalar coordinates -> squaredScalar
squaredMagnitude vector =
    let (Vector2d vx vy) = vector in vx * vx + vy * vy

direction :: Scalar scalar => Vector2d scalar coordinates -> Maybe (Direction2d coordinates)
direction vector =
    let m = magnitude vector
     in if m == Scalar.zero
            then Nothing
            else
                let (Vector2d vx vy) = vector
                 in Just (Direction2d (vx / m) (vy / m))

normalize :: Scalar scalar => Vector2d scalar coordinates -> Vector2d Float coordinates
normalize vector =
    let m = magnitude vector
     in if m == Scalar.zero
            then zero
            else
                let (Vector2d vx vy) = vector
                 in Vector2d (vx / m) (vy / m)
