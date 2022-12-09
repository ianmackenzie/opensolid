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

import Area qualified
import {-# SOURCE #-} Direction3d (Direction3d)
import {-# SOURCE #-} Direction3d qualified
import Length qualified
import OpenSolid hiding (zero)
import {-# SOURCE #-} Point3d (Point3d (..))
import Qty qualified
import Units qualified

data Vector3d units coordinates = Vector3d !(Qty units) !(Qty units) !(Qty units)
    deriving (Eq)

deriving instance Show (Qty units) => Show (Vector3d units coordinates)

instance Negation (Vector3d units coordinates) where
    negate (Vector3d vx vy vz) = Vector3d (negate vx) (negate vy) (negate vz)

instance Addition (Vector3d units) (Vector3d units) (Vector3d units) where
    (Vector3d x1 y1 z1) + (Vector3d x2 y2 z2) = Vector3d (x1 + x2) (y1 + y2) (z1 + z2)

instance Subtraction (Vector3d units) (Vector3d units) (Vector3d units) where
    (Vector3d x1 y1 z1) - (Vector3d x2 y2 z2) = Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Qty units1) (Vector3d units2 coordinates) (Vector3d units3 coordinates) where
    scale * (Vector3d vx vy vz) = Vector3d (scale * vx) (scale * vy) (scale * vz)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Vector3d units1 coordinates) (Qty units2) (Vector3d units3 coordinates) where
    (Vector3d vx vy vz) * scale = Vector3d (vx * scale) (vy * scale) (vz * scale)

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (Vector3d units1 coordinates) (Qty units2) (Vector3d units3 coordinates) where
    (Vector3d vx vy vz) / scale = Vector3d (vx / scale) (vy / scale) (vz / scale)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => DotProduct (Vector3d units1) (Vector3d units2) (Qty units3) where
    (Vector3d x1 y1 z1) <> (Vector3d x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => CrossProduct (Vector3d units1) (Vector3d units2) (Vector3d units3) where
    (Vector3d x1 y1 z1) >< (Vector3d x2 y2 z2) =
        Vector3d
            (y1 * z2 - z1 * y2)
            (z1 * x2 - x1 * z2)
            (x1 * y2 - y1 * x2)

zero :: Vector3d units coordinates
zero = Vector3d Qty.zero Qty.zero Qty.zero

x :: Qty units -> Vector3d units coordinates
x vx = Vector3d vx Qty.zero Qty.zero

y :: Qty units -> Vector3d units coordinates
y vy = Vector3d Qty.zero vy Qty.zero

z :: Qty units -> Vector3d units coordinates
z vz = Vector3d Qty.zero Qty.zero vz

xy :: Qty units -> Qty units -> Vector3d units coordinates
xy vx vz = Vector3d vx vz Qty.zero

xz :: Qty units -> Qty units -> Vector3d units coordinates
xz vx vz = Vector3d vx Qty.zero vz

yz :: Qty units -> Qty units -> Vector3d units coordinates
yz vy vz = Vector3d Qty.zero vy vz

xyz :: Qty units -> Qty units -> Qty units -> Vector3d units coordinates
xyz = Vector3d

meters :: Float -> Float -> Float -> Vector3d Meters coordinates
meters vx vy vz = Vector3d (Length.meters vx) (Length.meters vy) (Length.meters vz)

squareMeters :: Float -> Float -> Float -> Vector3d SquareMeters coordinates
squareMeters vx vy vz = Vector3d (Area.squareMeters vx) (Area.squareMeters vy) (Area.squareMeters vz)

from :: Point3d coordinates -> Point3d coordinates -> Vector3d Meters coordinates
from (Point3d x1 y1 z1) (Point3d x2 y2 z2) = Vector3d (x2 - x1) (y2 - y1) (z2 - z1)

interpolateFrom :: Vector3d units coordinates -> Vector3d units coordinates -> Float -> Vector3d units coordinates
interpolateFrom (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) t =
    Vector3d (Qty.interpolateFrom x1 x2 t) (Qty.interpolateFrom y1 y2 t) (Qty.interpolateFrom z1 z2 t)

midpoint :: Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates
midpoint (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) =
    Vector3d (Qty.midpoint x1 x2) (Qty.midpoint y1 y2) (Qty.midpoint z1 z2)

magnitude :: Vector3d units coordinates -> Qty units
magnitude (Vector3d vx vy vz) =
    let fx = Units.drop vx
        fy = Units.drop vy
        fz = Units.drop vz
     in Units.add (sqrt (fx * fx + fy * fy + fz * fz))

squaredMagnitude :: Multiplication (Qty units1) (Qty units1) (Qty units2) => Vector3d units1 coordinates -> Qty units2
squaredMagnitude (Vector3d vx vy vz) = vx * vx + vy * vy + vz * vz

direction :: Vector3d units coordinates -> Maybe (Direction3d coordinates)
direction vector =
    let m = magnitude vector
     in if m == Qty.zero
            then Nothing
            else
                let (Vector3d vx vy vz) = vector
                 in Just (Direction3d.unsafe (vx / m) (vy / m) (vz / m))

normalize :: Vector3d units coordinates -> Vector3d Unitless coordinates
normalize vector =
    let m = magnitude vector
     in if m == Qty.zero
            then zero
            else
                let (Vector3d vx vy vz) = vector
                 in Vector3d (vx / m) (vy / m) (vz / m)
