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
import qualified Scalar
import qualified Units
import Vector3d.Type

zero :: Scalar scalar => Vector3d scalar coordinates
zero =
    Vector3d Scalar.zero Scalar.zero Scalar.zero

x :: Scalar scalar => scalar -> Vector3d scalar coordinates
x vx =
    Vector3d vx Scalar.zero Scalar.zero

y :: Scalar scalar => scalar -> Vector3d scalar coordinates
y vy =
    Vector3d Scalar.zero vy Scalar.zero

z :: Scalar scalar => scalar -> Vector3d scalar coordinates
z vz =
    Vector3d Scalar.zero Scalar.zero vz

xy :: Scalar scalar => scalar -> scalar -> Vector3d scalar coordinates
xy vx vz =
    Vector3d vx vz Scalar.zero

xz :: Scalar scalar => scalar -> scalar -> Vector3d scalar coordinates
xz vx vz =
    Vector3d vx Scalar.zero vz

yz :: Scalar scalar => scalar -> scalar -> Vector3d scalar coordinates
yz vy vz =
    Vector3d Scalar.zero vy vz

xyz :: Scalar scalar => scalar -> scalar -> scalar -> Vector3d scalar coordinates
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

interpolateFrom :: Scalar scalar => Vector3d scalar coordinates -> Vector3d scalar coordinates -> Float -> Vector3d scalar coordinates
interpolateFrom v1 v2 t =
    let (Vector3d x1 y1 z1) = v1
        (Vector3d x2 y2 z2) = v2
        vx = Scalar.interpolateFrom x1 x2 t
        vy = Scalar.interpolateFrom y1 y2 t
        vz = Scalar.interpolateFrom z1 z2 t
     in Vector3d vx vy vz

midpoint :: Scalar scalar => Vector3d scalar coordinates -> Vector3d scalar coordinates -> Vector3d scalar coordinates
midpoint v1 v2 =
    let (Vector3d x1 y1 z1) = v1
        (Vector3d x2 y2 z2) = v2
        vx = Scalar.midpoint x1 x2
        vy = Scalar.midpoint y1 y2
        vz = Scalar.midpoint z1 z2
     in Vector3d vx vy vz

magnitude :: Scalar scalar => Vector3d scalar coordinates -> scalar
magnitude vector =
    let (Vector3d vx vy vz) = vector
        fx = Units.drop vx
        fy = Units.drop vy
        fz = Units.drop vz
     in Units.add (Float.sqrt (fx * fx + fy * fy + fz * fz))

squaredMagnitude :: (Scalar scalar, Scalar squaredScalar, Multiplication scalar scalar squaredScalar) => Vector3d scalar coordinates -> squaredScalar
squaredMagnitude vector =
    let (Vector3d vx vy vz) = vector in vx * vx + vy * vy + vz * vz

direction :: Scalar scalar => Vector3d scalar coordinates -> Maybe (Direction3d coordinates)
direction vector =
    let m = magnitude vector
     in if m == Scalar.zero
            then Nothing
            else
                let (Vector3d vx vy vz) = vector
                 in Just (Direction3d (vx / m) (vy / m) (vz / m))

normalize :: Scalar scalar => Vector3d scalar coordinates -> Vector3d Float coordinates
normalize vector =
    let m = magnitude vector
     in if m == Scalar.zero
            then zero
            else
                let (Vector3d vx vy vz) = vector
                 in Vector3d (vx / m) (vy / m) (vz / m)
