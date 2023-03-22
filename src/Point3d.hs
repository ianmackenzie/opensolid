module Point3d
  ( Point3d (Point3d, xCoordinate, yCoordinate, zCoordinate)
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
import CoordinateSystem (Units)
import Length qualified
import OpenSolid
import Qty qualified
import Range qualified
import Units (Meters)
import Vector3d (Vector3d (Vector3d))
import Vector3d qualified

data Point3d (coordinateSystem :: CoordinateSystem) = Point3d
  { xCoordinate :: Qty (Units coordinateSystem)
  , yCoordinate :: Qty (Units coordinateSystem)
  , zCoordinate :: Qty (Units coordinateSystem)
  }
  deriving (Eq, Show)

instance (space ~ space', units ~ units') => Addition (Point3d (Coordinates space units)) (Vector3d (Coordinates space' units')) (Point3d (Coordinates space units)) where
  Point3d px py pz + Vector3d vx vy vz = Point3d (px + vx) (py + vy) (pz + vz)

instance (space ~ space', units ~ units') => Subtraction (Point3d (Coordinates space units)) (Vector3d (Coordinates space' units')) (Point3d (Coordinates space units)) where
  Point3d px py pz - Vector3d vx vy vz = Point3d (px - vx) (py - vy) (pz - vz)

instance (space ~ space', units ~ units') => Subtraction (Point3d (Coordinates space units)) (Point3d (Coordinates space' units')) (Vector3d (Coordinates space units)) where
  Point3d x1 y1 z1 - Point3d x2 y2 z2 = Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  (space ~ space', units ~ units')
  => ApproximateEquality (Point3d (Coordinates space units)) (Point3d (Coordinates space' units')) units
  where
  p1 ~= p2 = distanceFrom p1 p2 ~= Qty.zero

instance Bounded (Point3d (Coordinates space units)) (BoundingBox3d (Coordinates space units)) where
  bounds (Point3d px py pz) =
    BoundingBox3d (Range.constant px) (Range.constant py) (Range.constant pz)

origin :: Point3d (Coordinates space units)
origin = Point3d Qty.zero Qty.zero Qty.zero

x :: Qty units -> Point3d (Coordinates space units)
x px = Point3d px Qty.zero Qty.zero

y :: Qty units -> Point3d (Coordinates space units)
y py = Point3d Qty.zero py Qty.zero

z :: Qty units -> Point3d (Coordinates space units)
z pz = Point3d Qty.zero Qty.zero pz

xy :: Qty units -> Qty units -> Point3d (Coordinates space units)
xy px py = Point3d px py Qty.zero

xz :: Qty units -> Qty units -> Point3d (Coordinates space units)
xz px pz = Point3d px Qty.zero pz

yz :: Qty units -> Qty units -> Point3d (Coordinates space units)
yz py pz = Point3d Qty.zero py pz

xyz :: Qty units -> Qty units -> Qty units -> Point3d (Coordinates space units)
xyz = Point3d

meters :: Float -> Float -> Float -> Point3d (Coordinates space Meters)
meters px py pz = Point3d (Length.meters px) (Length.meters py) (Length.meters pz)

interpolateFrom :: Point3d (Coordinates space units) -> Point3d (Coordinates space units) -> Float -> Point3d (Coordinates space units)
interpolateFrom (Point3d x1 y1 z1) (Point3d x2 y2 z2) t =
  Point3d (Qty.interpolateFrom x1 x2 t) (Qty.interpolateFrom y1 y2 t) (Qty.interpolateFrom z1 z2 t)

midpoint :: Point3d (Coordinates space units) -> Point3d (Coordinates space units) -> Point3d (Coordinates space units)
midpoint (Point3d x1 y1 z1) (Point3d x2 y2 z2) =
  Point3d (Qty.midpoint x1 x2) (Qty.midpoint y1 y2) (Qty.midpoint z1 z2)

distanceFrom :: Point3d (Coordinates space units) -> Point3d (Coordinates space units) -> Qty units
distanceFrom p1 p2 = Vector3d.magnitude (p2 - p1)
