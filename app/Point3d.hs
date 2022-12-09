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

import Bounded (Bounded (..))
import {-# SOURCE #-} BoundingBox3d (BoundingBox3d (..))
import Length qualified
import OpenSolid
import Qty qualified
import Range qualified
import Vector3d qualified

data Point3d coordinates = Point3d !Length !Length !Length
    deriving (Eq, Show)

instance Bounded (Point3d coordinates) (BoundingBox3d coordinates) where
    bounds (Point3d px py pz) = BoundingBox3d (Range.constant px) (Range.constant py) (Range.constant pz)

origin :: Point3d coordinates
origin = Point3d zero zero zero

x :: Length -> Point3d coordinates
x px = Point3d px zero zero

y :: Length -> Point3d coordinates
y py = Point3d zero py zero

z :: Length -> Point3d coordinates
z pz = Point3d zero zero pz

xy :: Length -> Length -> Point3d coordinates
xy px py = Point3d px py zero

xz :: Length -> Length -> Point3d coordinates
xz px pz = Point3d px zero pz

yz :: Length -> Length -> Point3d coordinates
yz py pz = Point3d zero py pz

xyz :: Length -> Length -> Length -> Point3d coordinates
xyz = Point3d

meters :: Float -> Float -> Float -> Point3d coordinates
meters px py pz = Point3d (Length.meters px) (Length.meters py) (Length.meters pz)

interpolateFrom :: Point3d coordinates -> Point3d coordinates -> Float -> Point3d coordinates
interpolateFrom (Point3d x1 y1 z1) (Point3d x2 y2 z2) t =
    Point3d (Qty.interpolateFrom x1 x2 t) (Qty.interpolateFrom y1 y2 t) (Qty.interpolateFrom z1 z2 t)

midpoint :: Point3d coordinates -> Point3d coordinates -> Point3d coordinates
midpoint (Point3d x1 y1 z1) (Point3d x2 y2 z2) =
    Point3d (Qty.midpoint x1 x2) (Qty.midpoint y1 y2) (Qty.midpoint z1 z2)

distanceFrom :: Point3d coordinates -> Point3d coordinates -> Length
distanceFrom p1 p2 = Vector3d.magnitude (Vector3d.from p1 p2)
