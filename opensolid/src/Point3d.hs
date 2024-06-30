module Point3d
  ( Point3d (Point3d)
  , xCoordinate
  , yCoordinate
  , zCoordinate
  , coordinates
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
  , placeIn
  , relativeTo
  , convert
  , unconvert
  , transformBy
  )
where

import Bounded qualified
import {-# SOURCE #-} Bounds3d (Bounds3d)
import {-# SOURCE #-} Bounds3d qualified
import Data.Coerce qualified
import {-# SOURCE #-} Frame3d (Frame3d)
import Length qualified
import OpenSolid
import Point3d.CoordinateTransformation qualified
import Qty qualified
import Transform3d (Transform3d (Transform3d))
import Units (Meters)
import Units qualified
import Vector3d (Vector3d (Vector3d))
import Vector3d qualified
import VectorBounds3d (VectorBounds3d (VectorBounds3d))

type role Point3d phantom

data Point3d (coordinateSystem :: CoordinateSystem) where
  Point3d ::
    Qty (Units coordinateSystem) ->
    Qty (Units coordinateSystem) ->
    Qty (Units coordinateSystem) ->
    Point3d coordinateSystem

deriving instance Eq (Point3d (space @ units))

deriving instance Show (Point3d (space @ units))

instance HasUnits (Point3d (space @ units)) where
  type Units (Point3d (space @ units)) = units
  type Erase (Point3d (space @ units)) = Point3d (space @ Unitless)

instance space ~ space_ => Units.Coercion (Point3d (space @ units1)) (Point3d (space_ @ units2)) where
  coerce = Data.Coerce.coerce

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Point3d (space @ units))
    (Vector3d (space_ @ units_))
    (Point3d (space @ units))
  where
  Point3d px py pz + Vector3d vx vy vz = Point3d (px + vx) (py + vy) (pz + vz)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Point3d (space @ units))
    (Vector3d (space_ @ units_))
    (Point3d (space @ units))
  where
  Point3d px py pz - Vector3d vx vy vz = Point3d (px - vx) (py - vy) (pz - vz)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Point3d (space @ units))
    (Point3d (space_ @ units_))
    (Vector3d (space @ units))
  where
  Point3d x1 y1 z1 - Point3d x2 y2 z2 = Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Point3d (space @ units))
    (VectorBounds3d (space_ @ units_))
    (Bounds3d (space @ units))
  where
  Point3d px py pz + VectorBounds3d vx vy vz = Bounds3d.xyz (px + vx) (py + vy) (pz + vz)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Point3d (space @ units))
    (VectorBounds3d (space_ @ units_))
    (Bounds3d (space @ units))
  where
  Point3d px py pz - VectorBounds3d vx vy vz = Bounds3d.xyz (px - vx) (py - vy) (pz - vz)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  ApproximateEquality (Point3d (space @ units)) (Point3d (space_ @ units_)) units
  where
  p1 ~= p2 = distanceFrom p1 p2 ~= Qty.zero

instance Bounded.Interface (Point3d (space @ units)) (Bounds3d (space @ units)) where
  bounds = Bounds3d.constant

xCoordinate :: Point3d (space @ units) -> Qty units
xCoordinate (Point3d px _ _) = px

yCoordinate :: Point3d (space @ units) -> Qty units
yCoordinate (Point3d _ py _) = py

zCoordinate :: Point3d (space @ units) -> Qty units
zCoordinate (Point3d _ _ pz) = pz

{-# INLINE coordinates #-}
coordinates :: Point3d (space @ units) -> (Qty units, Qty units, Qty units)
coordinates (Point3d px py pz) = (px, py, pz)

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

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Point3d (local @ units) ->
  Point3d (global @ units)
placeIn = Point3d.CoordinateTransformation.placeIn

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Point3d (global @ units) ->
  Point3d (local @ units)
relativeTo = Point3d.CoordinateTransformation.relativeTo

convert :: Qty (units2 :/: units1) -> Point3d (space @ units1) -> Point3d (space @ units2)
convert conversion (Point3d px py pz) =
  Point3d (Qty.convert conversion px) (Qty.convert conversion py) (Qty.convert conversion pz)

unconvert :: Qty (units2 :/: units1) -> Point3d (space @ units2) -> Point3d (space @ units1)
unconvert conversion (Point3d px py pz) =
  Point3d (Qty.unconvert conversion px) (Qty.unconvert conversion py) (Qty.unconvert conversion pz)

transformBy :: Transform3d tag (space @ units) -> Point3d (space @ units) -> Point3d (space @ units)
transformBy transform point = do
  let (Transform3d p0 vx vy vz) = transform
  let (px, py, pz) = coordinates point
  p0 + px * vx + py * vy + pz * vz
