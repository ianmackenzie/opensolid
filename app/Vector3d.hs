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
    midpoint,
    interpolateFrom,
    magnitude,
    squaredMagnitude,
    direction,
    normalize,
) where

import qualified Area
import Data.Coerce (coerce)
import Direction3d.Unsafe
import qualified Length
import OpenSolid
import qualified Quantity
import qualified Units
import Vector3d.Type

zero :: Vector3d units coordinates
zero =
    Vector3d Quantity.zero Quantity.zero Quantity.zero

x :: Quantity units -> Vector3d units coordinates
x vx =
    Vector3d vx Quantity.zero Quantity.zero

y :: Quantity units -> Vector3d units coordinates
y vy =
    Vector3d Quantity.zero vy Quantity.zero

z :: Quantity units -> Vector3d units coordinates
z vz =
    Vector3d Quantity.zero Quantity.zero vz

xy :: Quantity units -> Quantity units -> Vector3d units coordinates
xy vx vz =
    Vector3d vx vz Quantity.zero

xz :: Quantity units -> Quantity units -> Vector3d units coordinates
xz vx vz =
    Vector3d vx Quantity.zero vz

yz :: Quantity units -> Quantity units -> Vector3d units coordinates
yz vy vz =
    Vector3d Quantity.zero vy vz

xyz :: Quantity units -> Quantity units -> Quantity units -> Vector3d units coordinates
xyz =
    Vector3d

meters :: Float -> Float -> Float -> Vector3d Units.Meters coordinates
meters vx vy vz =
    Vector3d (Length.meters vx) (Length.meters vy) (Length.meters vz)

squareMeters :: Float -> Float -> Float -> Vector3d Units.SquareMeters coordinates
squareMeters vx vy vz =
    Vector3d (Area.squareMeters vx) (Area.squareMeters vy) (Area.squareMeters vz)

interpolateFrom :: Vector3d units coordinates -> Vector3d units coordinates -> Float -> Vector3d units coordinates
interpolateFrom v1 v2 t =
    let (Vector3d x1 y1 z1) = v1
        (Vector3d x2 y2 z2) = v2
        vx = Quantity.interpolateFrom x1 x2 t
        vy = Quantity.interpolateFrom y1 y2 t
        vz = Quantity.interpolateFrom z1 z2 t
     in Vector3d vx vy vz

midpoint :: Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates
midpoint v1 v2 =
    let (Vector3d x1 y1 z1) = v1
        (Vector3d x2 y2 z2) = v2
        vx = Quantity.midpoint x1 x2
        vy = Quantity.midpoint y1 y2
        vz = Quantity.midpoint z1 z2
     in Vector3d vx vy vz

magnitude :: Vector3d units coordinates -> Quantity units
magnitude vector =
    let (Vector3d vx vy vz) = coerce vector :: Vector3d Unitless coordinates
     in coerce (sqrt (vx * vx + vy * vy + vz * vz))

squaredMagnitude :: Units.Multiplication units units => Vector3d units coordinates -> Quantity (Units.Product units units)
squaredMagnitude vector =
    let (Vector3d vx vy vz) = vector in vx * vx + vy * vy + vz * vz

direction :: Vector3d units coordinates -> Maybe (Direction3d coordinates)
direction vector =
    let m = magnitude vector
     in if m == Quantity.zero
            then Nothing
            else
                let (Vector3d vx vy vz) = vector
                 in Just (Direction3d (vx / m) (vy / m) (vz / m))

normalize :: Vector3d units coordinates -> Vector3d Unitless coordinates
normalize vector =
    let m = magnitude vector
     in if m == Quantity.zero
            then zero
            else
                let (Vector3d vx vy vz) = vector
                 in Vector3d (vx / m) (vy / m) (vz / m)
