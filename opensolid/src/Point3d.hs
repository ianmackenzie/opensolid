module Point3d
  ( Point3d (Point3d)
  , xCoordinate
  , yCoordinate
  , zCoordinate
  , origin
  , x
  , y
  , z
  , xy
  , xz
  , yz
  , xyz
  , meters
  , centimeters
  , millimeters
  , inches
  , midpoint
  , interpolateFrom
  , distanceFrom
  )
where

import Bounded qualified
import {-# SOURCE #-} Bounds3d (Bounds3d (Bounds3d))
import {-# SOURCE #-} Bounds3d qualified
import Length qualified
import OpenSolid
import Qty qualified
import Units (Meters)
import Units qualified
import Vector3d (Vector3d (Vector3d))
import Vector3d qualified
import VectorBounds3d (VectorBounds3d (VectorBounds3d))

data Point3d (coordinateSystem :: CoordinateSystem) where
  Point3d :: Qty units -> Qty units -> Qty units -> Point3d (space @ units)

deriving instance Eq (Point3d (space @ units))

deriving instance Show (Point3d (space @ units))

instance
  (units1 ~ units1', units2 ~ units2', space ~ space') =>
  Units.Coercion
    units1
    units2
    (Point3d (space @ units1'))
    (Point3d (space' @ units2'))

instance
  (space ~ space', units ~ units') =>
  Addition
    (Point3d (space @ units))
    (Vector3d (space' @ units'))
    (Point3d (space @ units))
  where
  Point3d px py pz + Vector3d vx vy vz = Point3d (px + vx) (py + vy) (pz + vz)

instance
  (space ~ space', units ~ units') =>
  Subtraction
    (Point3d (space @ units))
    (Vector3d (space' @ units'))
    (Point3d (space @ units))
  where
  Point3d px py pz - Vector3d vx vy vz = Point3d (px - vx) (py - vy) (pz - vz)

instance
  (space ~ space', units ~ units') =>
  Subtraction
    (Point3d (space @ units))
    (Point3d (space' @ units'))
    (Vector3d (space @ units))
  where
  Point3d x1 y1 z1 - Point3d x2 y2 z2 = Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  (units ~ units', space ~ space') =>
  Addition
    (Point3d (space @ units))
    (VectorBounds3d (space' @ units'))
    (Bounds3d (space @ units))
  where
  Point3d px py pz + VectorBounds3d vx vy vz = Bounds3d (px + vx) (py + vy) (pz + vz)

instance
  (units ~ units', space ~ space') =>
  Subtraction
    (Point3d (space @ units))
    (VectorBounds3d (space' @ units'))
    (Bounds3d (space @ units))
  where
  Point3d px py pz - VectorBounds3d vx vy vz = Bounds3d (px - vx) (py - vy) (pz - vz)

instance
  (space ~ space', units ~ units') =>
  ApproximateEquality (Point3d (space @ units)) (Point3d (space' @ units')) units
  where
  p1 ~= p2 = distanceFrom p1 p2 ~= Qty.zero

instance Bounded.Interface (Point3d (space @ units)) (Bounds3d (space @ units)) where
  boundsImpl = Bounds3d.constant

xCoordinate :: Point3d (space @ units) -> Qty units
xCoordinate (Point3d px _ _) = px

yCoordinate :: Point3d (space @ units) -> Qty units
yCoordinate (Point3d _ py _) = py

zCoordinate :: Point3d (space @ units) -> Qty units
zCoordinate (Point3d _ _ pz) = pz

origin :: Point3d (space @ units)
origin = Point3d Qty.zero Qty.zero Qty.zero

x :: Qty units -> Point3d (space @ units)
x px = Point3d px Qty.zero Qty.zero

y :: Qty units -> Point3d (space @ units)
y py = Point3d Qty.zero py Qty.zero

z :: Qty units -> Point3d (space @ units)
z pz = Point3d Qty.zero Qty.zero pz

xy :: Qty units -> Qty units -> Point3d (space @ units)
xy px py = Point3d px py Qty.zero

xz :: Qty units -> Qty units -> Point3d (space @ units)
xz px pz = Point3d px Qty.zero pz

yz :: Qty units -> Qty units -> Point3d (space @ units)
yz py pz = Point3d Qty.zero py pz

xyz :: Qty units -> Qty units -> Qty units -> Point3d (space @ units)
xyz = Point3d

apply :: (Float -> Qty units) -> Float -> Float -> Float -> Point3d (space @ units)
apply units px py pz = Point3d (units px) (units py) (units pz)

meters :: Float -> Float -> Float -> Point3d (space @ Meters)
meters = apply Length.meters

centimeters :: Float -> Float -> Float -> Point3d (space @ Meters)
centimeters = apply Length.centimeters

millimeters :: Float -> Float -> Float -> Point3d (space @ Meters)
millimeters = apply Length.millimeters

inches :: Float -> Float -> Float -> Point3d (space @ Meters)
inches = apply Length.inches

interpolateFrom ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Float ->
  Point3d (space @ units)
interpolateFrom (Point3d x1 y1 z1) (Point3d x2 y2 z2) t =
  Point3d
    (Qty.interpolateFrom x1 x2 t)
    (Qty.interpolateFrom y1 y2 t)
    (Qty.interpolateFrom z1 z2 t)

midpoint :: Point3d (space @ units) -> Point3d (space @ units) -> Point3d (space @ units)
midpoint (Point3d x1 y1 z1) (Point3d x2 y2 z2) =
  Point3d (Qty.midpoint x1 x2) (Qty.midpoint y1 y2) (Qty.midpoint z1 z2)

distanceFrom :: Point3d (space @ units) -> Point3d (space @ units) -> Qty units
distanceFrom p1 p2 = Vector3d.magnitude (p2 - p1)
