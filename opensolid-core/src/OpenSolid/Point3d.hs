module OpenSolid.Point3d
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

import {-# SOURCE #-} OpenSolid.Bounds3d (Bounds3d)
import {-# SOURCE #-} OpenSolid.Bounds3d qualified as Bounds3d
import {-# SOURCE #-} OpenSolid.Frame3d (Frame3d)
import OpenSolid.Length qualified as Length
import OpenSolid.Point3d.CoordinateTransformation qualified as Point3d.CoordinateTransformation
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Transform3d (Transform3d (Transform3d))
import OpenSolid.Units (Meters)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d (Vector3d))
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds3d (VectorBounds3d (VectorBounds3d))

type role Point3d nominal

data Point3d (coordinateSystem :: CoordinateSystem) where
  Point3d ::
    Qty units ->
    Qty units ->
    Qty units ->
    Point3d (space @ units)

deriving instance Eq (Point3d (space @ units))

deriving instance Show (Point3d (space @ units))

instance HasUnits (Point3d (space @ units)) units (Point3d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion (Point3d (space1 @ unitsA)) (Point3d (space2 @ unitsB))
  where
  coerce (Point3d px py pz) = Point3d (Units.coerce px) (Units.coerce py) (Units.coerce pz)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Point3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Point3d (space1 @ units1))
  where
  Point3d px py pz + Vector3d vx vy vz = Point3d (px + vx) (py + vy) (pz + vz)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Point3d (space1 @ units1))
  where
  Point3d px py pz - Vector3d vx vy vz = Point3d (px - vx) (py - vy) (pz - vz)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Point3d (space2 @ units2))
    (Vector3d (space1 @ units1))
  where
  Point3d x1 y1 z1 - Point3d x2 y2 z2 = Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Point3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (Bounds3d (space1 @ units1))
  where
  Point3d px py pz + VectorBounds3d vx vy vz = Bounds3d.xyz (px + vx) (py + vy) (pz + vz)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (Bounds3d (space1 @ units1))
  where
  Point3d px py pz - VectorBounds3d vx vy vz = Bounds3d.xyz (px - vx) (py - vy) (pz - vz)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  ApproximateEquality (Point3d (space1 @ units1)) (Point3d (space2 @ units2)) units1
  where
  p1 ~= p2 = distanceFrom p1 p2 ~= Qty.zero

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
