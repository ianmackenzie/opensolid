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
    distanceFrom,
) where

import qualified Length
import OpenSolid
import Point3d.Type
import qualified Qty
import qualified Vector3d

origin :: Point3d coordinates
origin =
    Point3d zero zero zero

x :: Length -> Point3d coordinates
x px =
    Point3d px zero zero

y :: Length -> Point3d coordinates
y py =
    Point3d zero py zero

z :: Length -> Point3d coordinates
z pz =
    Point3d zero zero pz

xy :: Length -> Length -> Point3d coordinates
xy px py =
    Point3d px py zero

xz :: Length -> Length -> Point3d coordinates
xz px pz =
    Point3d px zero pz

yz :: Length -> Length -> Point3d coordinates
yz py pz =
    Point3d zero py pz

xyz :: Length -> Length -> Length -> Point3d coordinates
xyz =
    Point3d

meters :: Float -> Float -> Float -> Point3d coordinates
meters px py pz =
    Point3d (Length.meters px) (Length.meters py) (Length.meters pz)

interpolateFrom :: Point3d coordinates -> Point3d coordinates -> Float -> Point3d coordinates
interpolateFrom p1 p2 t =
    let (Point3d x1 y1 z1) = p1
        (Point3d x2 y2 z2) = p2
        px = Qty.interpolateFrom x1 x2 t
        py = Qty.interpolateFrom y1 y2 t
        pz = Qty.interpolateFrom z1 z2 t
     in Point3d px py pz

midpoint :: Point3d coordinates -> Point3d coordinates -> Point3d coordinates
midpoint p1 p2 =
    let (Point3d x1 y1 z1) = p1
        (Point3d x2 y2 z2) = p2
        px = Qty.midpoint x1 x2
        py = Qty.midpoint y1 y2
        pz = Qty.midpoint z1 z2
     in Point3d px py pz

distanceFrom :: Point3d coordinates -> Point3d coordinates -> Length
distanceFrom p1 p2 =
    Vector3d.magnitude (Vector3d.from p1 p2)
