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
import qualified Float
import qualified Length
import OpenSolid
import Point3d.Type
import qualified Qty
import qualified Units
import Vector3d.Type

zero :: Vector3d (Qty a) coordinates
zero =
    Vector3d Qty.zero Qty.zero Qty.zero

x :: Qty a -> Vector3d (Qty a) coordinates
x vx =
    Vector3d vx Qty.zero Qty.zero

y :: Qty a -> Vector3d (Qty a) coordinates
y vy =
    Vector3d Qty.zero vy Qty.zero

z :: Qty a -> Vector3d (Qty a) coordinates
z vz =
    Vector3d Qty.zero Qty.zero vz

xy :: Qty a -> Qty a -> Vector3d (Qty a) coordinates
xy vx vz =
    Vector3d vx vz Qty.zero

xz :: Qty a -> Qty a -> Vector3d (Qty a) coordinates
xz vx vz =
    Vector3d vx Qty.zero vz

yz :: Qty a -> Qty a -> Vector3d (Qty a) coordinates
yz vy vz =
    Vector3d Qty.zero vy vz

xyz :: Qty a -> Qty a -> Qty a -> Vector3d (Qty a) coordinates
xyz =
    Vector3d

meters :: Float -> Float -> Float -> Vector3d Length coordinates
meters vx vy vz =
    Vector3d (Length.meters vx) (Length.meters vy) (Length.meters vz)

squareMeters :: Float -> Float -> Float -> Vector3d Area coordinates
squareMeters vx vy vz =
    Vector3d (Area.squareMeters vx) (Area.squareMeters vy) (Area.squareMeters vz)

from :: Point3d coordinates -> Point3d coordinates -> Vector3d Length coordinates
from p1 p2 =
    let (Point3d x1 y1 z1) = p1
        (Point3d x2 y2 z2) = p2
     in Vector3d (x2 - x1) (y2 - y1) (z2 - z1)

interpolateFrom :: Vector3d (Qty a) coordinates -> Vector3d (Qty a) coordinates -> Float -> Vector3d (Qty a) coordinates
interpolateFrom v1 v2 t =
    let (Vector3d x1 y1 z1) = v1
        (Vector3d x2 y2 z2) = v2
        vx = Qty.interpolateFrom x1 x2 t
        vy = Qty.interpolateFrom y1 y2 t
        vz = Qty.interpolateFrom z1 z2 t
     in Vector3d vx vy vz

midpoint :: Vector3d (Qty a) coordinates -> Vector3d (Qty a) coordinates -> Vector3d (Qty a) coordinates
midpoint v1 v2 =
    let (Vector3d x1 y1 z1) = v1
        (Vector3d x2 y2 z2) = v2
        vx = Qty.midpoint x1 x2
        vy = Qty.midpoint y1 y2
        vz = Qty.midpoint z1 z2
     in Vector3d vx vy vz

magnitude :: Vector3d (Qty a) coordinates -> Qty a
magnitude vector =
    let (Vector3d vx vy vz) = vector
        fx = Units.drop vx
        fy = Units.drop vy
        fz = Units.drop vz
     in Units.add (Float.sqrt (fx * fx + fy * fy + fz * fz))

squaredMagnitude :: Multiplication (Qty a) (Qty a) (Qty b) => Vector3d (Qty a) coordinates -> Qty b
squaredMagnitude vector =
    let (Vector3d vx vy vz) = vector in vx * vx + vy * vy + vz * vz

direction :: Vector3d (Qty a) coordinates -> Maybe (Direction3d coordinates)
direction vector =
    let m = magnitude vector
     in if m == Qty.zero
            then Nothing
            else
                let (Vector3d vx vy vz) = vector
                 in Just (Direction3d (vx / m) (vy / m) (vz / m))

normalize :: Vector3d (Qty a) coordinates -> Vector3d Float coordinates
normalize vector =
    let m = magnitude vector
     in if m == Qty.zero
            then zero
            else
                let (Vector3d vx vy vz) = vector
                 in Vector3d (vx / m) (vy / m) (vz / m)
