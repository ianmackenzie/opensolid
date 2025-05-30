module OpenSolid.Point3d
  ( Point3d
  , coordinates
  , origin
  , rightwardForwardUpward
  , on
  , along
  , coerce
  , erase
  , fromCoordinates
  , midpoint
  , interpolateFrom
  , distanceFrom
  , distanceAlong
  , placeIn
  , relativeTo
  , projectOnto
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
import OpenSolid.Basis3d qualified as Basis3d
import OpenSolid.Convention3d (Convention3d)
import OpenSolid.Convention3d qualified as Convention3d
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3d (Axis3d)
  , Basis3d (Basis3d)
  , Direction3d (Direction3d)
  , Frame3d (Frame3d)
  , PlanarBasis3d (PlanarBasis3d)
  , Plane3d (Plane3d)
  , Point2d (Point2d)
  , Point3d (Point3d)
  , Transform3d (Transform3d)
  , Vector3d
  )
import OpenSolid.Qty qualified as Qty
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Vector3d qualified as Vector3d

-- | Get the XYZ coordinates of a point, given an XYZ coordinate convention to use.
coordinates :: Convention3d -> Point3d (space @ units) -> (Qty units, Qty units, Qty units)
coordinates convention (Point3d pR pF pU) = do
  let Direction3d iR iF iU = Convention3d.xDirection Basis3d.world convention
  let Direction3d jR jF jU = Convention3d.yDirection Basis3d.world convention
  let Direction3d kR kF kU = Convention3d.zDirection Basis3d.world convention
  let pX = pR * iR + pF * iF + pU * iU
  let pY = pR * jR + pF * jF + pU * jU
  let pZ = pR * kR + pF * kF + pU * kU
  (pX, pY, pZ)

-- | The point with coordinates (0,0, 0).
origin :: Point3d (space @ units)
origin = Point3d Qty.zero Qty.zero Qty.zero

{-# INLINE rightwardForwardUpward #-}
rightwardForwardUpward :: Qty units -> Qty units -> Qty units -> Point3d (space @ units)
rightwardForwardUpward = Point3d

-- | Construct a point the given distance along the given axis.
along :: Axis3d (space @ units) -> Qty units -> Point3d (space @ units)
along (Axis3d originPoint direction) distance = do
  let Point3d oR oF oU = originPoint
  let Direction3d dR dF dU = direction
  Point3d
    # oR + dR * distance
    # oF + dF * distance
    # oU + dU * distance

-- | Construct a point on the given plane, at the given position within the plane.
on :: Plane3d (space @ units) (Defines local) -> Point2d (local @ units) -> Point3d (space @ units)
on (Plane3d originPoint (PlanarBasis3d i j)) (Point2d pX pY) = do
  let Point3d oR oF oU = originPoint
  let Direction3d iR iF iU = i
  let Direction3d jR jF jU = j
  Point3d
    # oR + pX * iR + pY * jR
    # oF + pX * iF + pY * jF
    # oU + pX * iU + pY * jU

{-# INLINE coerce #-}
coerce :: Point3d (space1 @ units1) -> Point3d (space2 @ units2)
coerce (Point3d pR pF pU) = Point3d (Qty.coerce pR) (Qty.coerce pF) (Qty.coerce pU)

erase :: Point3d (space @ units) -> Point3d (space @ Unitless)
erase = coerce

-- | Construct a point from its XYZ coordinates, given the coordinate convention to use.
fromCoordinates :: Convention3d -> (Qty units, Qty units, Qty units) -> Point3d (space @ units)
fromCoordinates convention (pX, pY, pZ) = do
  let Direction3d iR iF iU = Convention3d.xDirection Basis3d.world convention
  let Direction3d jR jF jU = Convention3d.yDirection Basis3d.world convention
  let Direction3d kR kF kU = Convention3d.zDirection Basis3d.world convention
  Point3d
    # pX * iR + pY * jR + pZ * kR
    # pX * iF + pY * jF + pZ * kF
    # pX * iU + pY * jU + pZ * kU

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

-- | Find the midpoint between two points.
midpoint :: Point3d (space @ units) -> Point3d (space @ units) -> Point3d (space @ units)
midpoint (Point3d x1 y1 z1) (Point3d x2 y2 z2) =
  Point3d (Qty.midpoint x1 x2) (Qty.midpoint y1 y2) (Qty.midpoint z1 z2)

-- | Compute the distance from one point to another.
distanceFrom :: Point3d (space @ units) -> Point3d (space @ units) -> Qty units
distanceFrom p1 p2 = Vector3d.magnitude (p2 - p1)

{-| Compute the (signed) distance of a point along an axis.

This is the position along the axis of the given point projected onto the axis.
-}
distanceAlong :: Axis3d (space @ units) -> Point3d (space @ units) -> Qty units
distanceAlong (Axis3d p0 d) p = (p - p0) `dot` d

-- | Convert a point defined in local coordinates to one defined in global coordinates.
placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Point3d (local @ units) ->
  Point3d (global @ units)
placeIn (Frame3d p0 (Basis3d i j k)) (Point3d px py pz) = p0 + px * i + py * j + pz * k

-- | Convert a point defined in global coordinates to one defined in local coordinates.
relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Point3d (global @ units) ->
  Point3d (local @ units)
relativeTo (Frame3d p0 (Basis3d i j k)) p =
  let d = p - p0 in Point3d (d `dot` i) (d `dot` j) (d `dot` k)

-- | Project a point onto a plane.
projectOnto ::
  Plane3d (space @ units) (Defines localSpace) ->
  Point3d (space @ units) ->
  Point3d (space @ units)
projectOnto plane point = on plane (projectInto plane point)

{-| Project a point *into* a plane.

Conceptualy, this projects the point onto the plane in 3D,
then expresses the projected point in 2D planar XY coordinates.
-}
projectInto ::
  Plane3d (space @ units) (Defines localSpace) ->
  Point3d (space @ units) ->
  Point2d (localSpace @ units)
projectInto (Plane3d p0 (PlanarBasis3d i j)) p =
  let d = p - p0 in Point2d (d `dot` i) (d `dot` j)

convert :: Qty (units2 :/: units1) -> Point3d (space @ units1) -> Point3d (space @ units2)
convert factor (Point3d px py pz) = Point3d (px !* factor) (py !* factor) (pz !* factor)

unconvert :: Qty (units2 :/: units1) -> Point3d (space @ units2) -> Point3d (space @ units1)
unconvert factor (Point3d px py pz) = Point3d (px !/ factor) (py !/ factor) (pz !/ factor)

transformBy :: Transform3d tag (space @ units) -> Point3d (space @ units) -> Point3d (space @ units)
transformBy transform (Point3d px py pz) = do
  let (Transform3d p0 vx vy vz) = transform
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
