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
  , xyOn
  , meters
  , centimeters
  , millimeters
  , inches
  , midpoint
  , interpolateFrom
  , distanceFrom
  , placeIn
  , relativeTo
  , projectInto
  , convert
  , unconvert
  , transformBy
  , translateBy
  , translateIn
  , translateAlong
  , rotateAround
  , mirrorAcross
  , scaleAbout
  , scaleAlong
  , translateByOwn
  , translateInOwn
  , translateAlongOwn
  , rotateAroundOwn
  , mirrorAcrossOwn
  , scaleAboutOwn
  , scaleAlongOwn
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Length qualified as Length
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3d
  , Basis3d (Basis3d)
  , Direction3d
  , Frame3d (Frame3d)
  , Plane3d (Plane3d)
  , Point2d (Point2d)
  , Point3d (Point3d)
  , Transform3d (Transform3d)
  , Vector3d
  )
import OpenSolid.Qty qualified as Qty
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Units (Meters)
import OpenSolid.Vector3d qualified as Vector3d

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

xyOn :: Plane3d (space @ units) (Defines local) -> Qty units -> Qty units -> Point3d (space @ units)
xyOn (Plane3d p0 (Basis3d i j _)) px py = p0 + px * i + py * j

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
placeIn (Frame3d p0 (Basis3d i j k)) (Point3d px py pz) = p0 + px * i + py * j + pz * k

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Point3d (global @ units) ->
  Point3d (local @ units)
relativeTo (Frame3d p0 (Basis3d i j k)) p = let d = p - p0 in Point3d (d <> i) (d <> j) (d <> k)

projectInto ::
  Plane3d (space @ units) (Defines localSpace) ->
  Point3d (space @ units) ->
  Point2d (localSpace @ units)
projectInto (Plane3d p0 (Basis3d i j _)) p = let d = p - p0 in Point2d (d <> i) (d <> j)

convert :: Qty (units2 :/: units1) -> Point3d (space @ units1) -> Point3d (space @ units2)
convert factor (Point3d px py pz) = Point3d (px !* factor) (py !* factor) (pz !* factor)

unconvert :: Qty (units2 :/: units1) -> Point3d (space @ units2) -> Point3d (space @ units1)
unconvert factor (Point3d px py pz) = Point3d (px !/ factor) (py !/ factor) (pz !/ factor)

transformBy :: Transform3d tag (space @ units) -> Point3d (space @ units) -> Point3d (space @ units)
transformBy transform point = do
  let (Transform3d p0 vx vy vz) = transform
  let (px, py, pz) = coordinates point
  p0 + px * vx + py * vy + pz * vz

translateBy :: Vector3d (space @ units) -> Point3d (space @ units) -> Point3d (space @ units)
translateBy = Transform3d.translateByImpl transformBy

translateIn :: Direction3d space -> Qty units -> Point3d (space @ units) -> Point3d (space @ units)
translateIn = Transform3d.translateInImpl transformBy

translateAlong :: Axis3d (space @ units) -> Qty units -> Point3d (space @ units) -> Point3d (space @ units)
translateAlong = Transform3d.translateAlongImpl transformBy

rotateAround :: Axis3d (space @ units) -> Angle -> Point3d (space @ units) -> Point3d (space @ units)
rotateAround = Transform3d.rotateAroundImpl transformBy

mirrorAcross :: Plane3d (space @ units) defines -> Point3d (space @ units) -> Point3d (space @ units)
mirrorAcross = Transform3d.mirrorAcrossImpl transformBy

scaleAbout :: Point3d (space @ units) -> Float -> Point3d (space @ units) -> Point3d (space @ units)
scaleAbout = Transform3d.scaleAboutImpl transformBy

scaleAlong :: Axis3d (space @ units) -> Float -> Point3d (space @ units) -> Point3d (space @ units)
scaleAlong = Transform3d.scaleAlongImpl transformBy

translateByOwn :: (Point3d (space @ units) -> Vector3d (space @ units)) -> Point3d (space @ units) -> Point3d (space @ units)
translateByOwn = Transform3d.translateByOwnImpl transformBy

translateInOwn :: (Point3d (space @ units) -> Direction3d space) -> Qty units -> Point3d (space @ units) -> Point3d (space @ units)
translateInOwn = Transform3d.translateInOwnImpl transformBy

translateAlongOwn :: (Point3d (space @ units) -> Axis3d (space @ units)) -> Qty units -> Point3d (space @ units) -> Point3d (space @ units)
translateAlongOwn = Transform3d.translateAlongOwnImpl transformBy

rotateAroundOwn :: (Point3d (space @ units) -> Axis3d (space @ units)) -> Angle -> Point3d (space @ units) -> Point3d (space @ units)
rotateAroundOwn = Transform3d.rotateAroundOwnImpl transformBy

mirrorAcrossOwn :: (Point3d (space @ units) -> Plane3d (space @ units) defines) -> Point3d (space @ units) -> Point3d (space @ units)
mirrorAcrossOwn = Transform3d.mirrorAcrossOwnImpl transformBy

scaleAboutOwn :: (Point3d (space @ units) -> Point3d (space @ units)) -> Float -> Point3d (space @ units) -> Point3d (space @ units)
scaleAboutOwn = Transform3d.scaleAboutOwnImpl transformBy

scaleAlongOwn :: (Point3d (space @ units) -> Axis3d (space @ units)) -> Float -> Point3d (space @ units) -> Point3d (space @ units)
scaleAlongOwn = Transform3d.scaleAlongOwnImpl transformBy
