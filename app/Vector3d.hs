module Vector3d (
    Vector3d (..),
    zero,
    x,
    y,
    z,
    xy,
    xz,
    yz,
    xyz,
    meters,
    squareMeters,
    from,
    midpoint,
    interpolateFrom,
    magnitude,
    squaredMagnitude,
    direction,
    normalize,
) where

import qualified Area
import Direction3d.Unsafe
import qualified Length
import OpenSolid
import Point3d.Type
import qualified Qty
import qualified Units
import Vector3d.Type

zero :: Vector3d units coordinates
zero =
    Vector3d Qty.zero Qty.zero Qty.zero

x :: Qty units -> Vector3d units coordinates
x vx =
    Vector3d vx Qty.zero Qty.zero

y :: Qty units -> Vector3d units coordinates
y vy =
    Vector3d Qty.zero vy Qty.zero

z :: Qty units -> Vector3d units coordinates
z vz =
    Vector3d Qty.zero Qty.zero vz

xy :: Qty units -> Qty units -> Vector3d units coordinates
xy vx vz =
    Vector3d vx vz Qty.zero

xz :: Qty units -> Qty units -> Vector3d units coordinates
xz vx vz =
    Vector3d vx Qty.zero vz

yz :: Qty units -> Qty units -> Vector3d units coordinates
yz vy vz =
    Vector3d Qty.zero vy vz

xyz :: Qty units -> Qty units -> Qty units -> Vector3d units coordinates
xyz =
    Vector3d

meters :: Float -> Float -> Float -> Vector3d Meters coordinates
meters vx vy vz =
    Vector3d (Length.meters vx) (Length.meters vy) (Length.meters vz)

squareMeters :: Float -> Float -> Float -> Vector3d SquareMeters coordinates
squareMeters vx vy vz =
    Vector3d (Area.squareMeters vx) (Area.squareMeters vy) (Area.squareMeters vz)

from :: Point3d coordinates -> Point3d coordinates -> Vector3d Meters coordinates
from p1 p2 =
    let (Point3d x1 y1 z1) = p1
        (Point3d x2 y2 z2) = p2
     in Vector3d (x2 - x1) (y2 - y1) (z2 - z1)

interpolateFrom :: Vector3d units coordinates -> Vector3d units coordinates -> Float -> Vector3d units coordinates
interpolateFrom v1 v2 t =
    let (Vector3d x1 y1 z1) = v1
        (Vector3d x2 y2 z2) = v2
        vx = Qty.interpolateFrom x1 x2 t
        vy = Qty.interpolateFrom y1 y2 t
        vz = Qty.interpolateFrom z1 z2 t
     in Vector3d vx vy vz

midpoint :: Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates
midpoint v1 v2 =
    let (Vector3d x1 y1 z1) = v1
        (Vector3d x2 y2 z2) = v2
        vx = Qty.midpoint x1 x2
        vy = Qty.midpoint y1 y2
        vz = Qty.midpoint z1 z2
     in Vector3d vx vy vz

magnitude :: Vector3d units coordinates -> Qty units
magnitude vector =
    let (Vector3d vx vy vz) = vector
        fx = Units.drop vx
        fy = Units.drop vy
        fz = Units.drop vz
     in Units.add (Qty.sqrt (fx * fx + fy * fy + fz * fz))

squaredMagnitude :: Multiplication (Qty units1) (Qty units1) (Qty units2) => Vector3d units1 coordinates -> Qty units2
squaredMagnitude vector =
    let (Vector3d vx vy vz) = vector in vx * vx + vy * vy + vz * vz

direction :: Vector3d units coordinates -> Maybe (Direction3d coordinates)
direction vector =
    let m = magnitude vector
     in if m == Qty.zero
            then Nothing
            else
                let (Vector3d vx vy vz) = vector
                 in Just (Direction3d (vx / m) (vy / m) (vz / m))

normalize :: Vector3d units coordinates -> Vector3d Unitless coordinates
normalize vector =
    let m = magnitude vector
     in if m == Qty.zero
            then zero
            else
                let (Vector3d vx vy vz) = vector
                 in Vector3d (vx / m) (vy / m) (vz / m)
