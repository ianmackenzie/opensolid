module Point3d
  ( Point3d (..)
  , origin
  , x
  , y
  , z
  , xy
  , xz
  , yz
  , xyz
  , meters
  , midpoint
  , interpolateFrom
  , distanceFrom
  )
where

import Bounded (Bounded (..))
import {-# SOURCE #-} BoundingBox3d (BoundingBox3d (..))
import Length qualified
import OpenSolid
import Qty qualified
import Range qualified
import Vector3d (Vector3d (Vector3d))
import Vector3d qualified

data Point3d units coordinates = Point3d (Qty units) (Qty units) (Qty units)
  deriving (Eq, Show)

instance (units ~ units', coordinates ~ coordinates') => Addition (Point3d units coordinates) (Vector3d units' coordinates') (Point3d units coordinates) where
  Point3d px py pz + Vector3d vx vy vz = Point3d (px + vx) (py + vy) (pz + vz)

instance (units ~ units', coordinates ~ coordinates') => Subtraction (Point3d units coordinates) (Vector3d units' coordinates') (Point3d units coordinates) where
  Point3d px py pz - Vector3d vx vy vz = Point3d (px - vx) (py - vy) (pz - vz)

instance (units ~ units', coordinates ~ coordinates') => Subtraction (Point3d units coordinates) (Point3d units' coordinates') (Vector3d units coordinates) where
  Point3d x1 y1 z1 - Point3d x2 y2 z2 = Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance ApproximateEquality (Point3d units coordinates) units where
  p1 ~= p2 = distanceFrom p1 p2 ~= Qty.zero

instance Bounded (Point3d units coordinates) (BoundingBox3d units coordinates) where
  bounds (Point3d px py pz) =
    BoundingBox3d (Range.constant px) (Range.constant py) (Range.constant pz)

origin :: Point3d units coordinates
origin = Point3d Qty.zero Qty.zero Qty.zero

x :: Qty units -> Point3d units coordinates
x px = Point3d px Qty.zero Qty.zero

y :: Qty units -> Point3d units coordinates
y py = Point3d Qty.zero py Qty.zero

z :: Qty units -> Point3d units coordinates
z pz = Point3d Qty.zero Qty.zero pz

xy :: Qty units -> Qty units -> Point3d units coordinates
xy px py = Point3d px py Qty.zero

xz :: Qty units -> Qty units -> Point3d units coordinates
xz px pz = Point3d px Qty.zero pz

yz :: Qty units -> Qty units -> Point3d units coordinates
yz py pz = Point3d Qty.zero py pz

xyz :: Qty units -> Qty units -> Qty units -> Point3d units coordinates
xyz = Point3d

meters :: Float -> Float -> Float -> Point3d Meters coordinates
meters px py pz = Point3d (Length.meters px) (Length.meters py) (Length.meters pz)

interpolateFrom :: Point3d units coordinates -> Point3d units coordinates -> Float -> Point3d units coordinates
interpolateFrom (Point3d x1 y1 z1) (Point3d x2 y2 z2) t =
  Point3d (Qty.interpolateFrom x1 x2 t) (Qty.interpolateFrom y1 y2 t) (Qty.interpolateFrom z1 z2 t)

midpoint :: Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
midpoint (Point3d x1 y1 z1) (Point3d x2 y2 z2) =
  Point3d (Qty.midpoint x1 x2) (Qty.midpoint y1 y2) (Qty.midpoint z1 z2)

distanceFrom :: Point3d units coordinates -> Point3d units coordinates -> Qty units
distanceFrom p1 p2 = Vector3d.magnitude (p2 - p1)
