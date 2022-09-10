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
    midpoint,
    interpolateFrom,
) where

import Length (Length, Meters)
import qualified Length
import OpenSolid
import qualified Quantity
import qualified Show
import qualified String
import Vector3d (Vector3d (..))
import qualified Vector3d

data Point3d coordinates = Point3d Length Length Length
    deriving (Eq)

instance Show (Point3d coordinates) where
    showsPrec precedence (Point3d x y z) =
        Show.primitive precedence "Point3d.meters" [Length.inMeters x, Length.inMeters y, Length.inMeters z]

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

interpolateFrom :: Point3d coordinates -> Point3d coordinates -> Float -> Point3d coordinates
interpolateFrom p1 p2 t =
    let (Point3d x1 y1 z1) = p1
        (Point3d x2 y2 z2) = p2
        x = Quantity.interpolateFrom x1 x2 t
        y = Quantity.interpolateFrom y1 y2 t
        z = Quantity.interpolateFrom z1 z2 t
     in Point3d x y z

midpoint :: Point3d coordinates -> Point3d coordinates -> Point3d coordinates
midpoint p1 p2 =
    let (Point3d x1 y1 z1) = p1
        (Point3d x2 y2 z2) = p2
        x = Quantity.midpoint x1 x2
        y = Quantity.midpoint y1 y2
        z = Quantity.midpoint z1 z2
     in Point3d x y z
