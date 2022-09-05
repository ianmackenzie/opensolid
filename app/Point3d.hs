module Point3d (
    Point3d (..),
    origin,
    x,
    y,
    z,
    xy,
    xz,
    yz,
    xyz,
    meters,
) where

import Length (Length, Meters)
import qualified Length
import OpenSolid
import qualified String
import Vector3d (Vector3d (..))
import qualified Vector3d

data Point3d coordinates = Point3d Length Length Length
    deriving (Eq)

instance Show (Point3d coordinates) where
    show (Point3d x y z) =
        let xString = String.fromFloat (Length.inMeters x)
            yString = String.fromFloat (Length.inMeters y)
            zString = String.fromFloat (Length.inMeters z)
         in String.toList ("Point3d.meters " ++ xString ++ " " ++ yString ++ " " ++ zString)

instance Addition Point3d (Vector3d Meters) where
    type Sum Point3d (Vector3d Meters) = Point3d
    (Point3d px py pz) + (Vector3d vx vy vz) =
        Point3d (px + vx) (py + vy) (pz + vz)

instance Subtraction Point3d (Vector3d Meters) where
    type Difference Point3d (Vector3d Meters) = Point3d
    (Point3d px py pz) - (Vector3d vx vy vz) =
        Point3d (px - vx) (py - vy) (pz - vz)

instance Subtraction Point3d Point3d where
    type Difference Point3d Point3d = Vector3d Meters
    (Point3d x1 y1 z1) - (Point3d x2 y2 z2) =
        Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

origin :: Point3d coordinates
origin =
    Point3d zero zero zero

x :: Length -> Point3d coordinates
x x =
    Point3d x zero zero

y :: Length -> Point3d coordinates
y y =
    Point3d zero y zero

z :: Length -> Point3d coordinates
z z =
    Point3d zero zero z

xy :: Length -> Length -> Point3d coordinates
xy x y =
    Point3d x y zero

xz :: Length -> Length -> Point3d coordinates
xz x z =
    Point3d x zero z

yz :: Length -> Length -> Point3d coordinates
yz y z =
    Point3d zero y z

xyz :: Length -> Length -> Length -> Point3d coordinates
xyz =
    Point3d

meters :: Float -> Float -> Float -> Point3d coordinates
meters x y z =
    Point3d (Length.meters x) (Length.meters y) (Length.meters z)
