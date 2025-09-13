module OpenSolid.Point2d
  ( Point2d (Point2d)
  , origin
  , coerce
  , x
  , y
  , along
  , polar
  , meters
  , centimeters
  , cm
  , millimeters
  , mm
  , inches
  , xCoordinate
  , yCoordinate
  , coordinates
  , midpoint
  , interpolateFrom
  , distanceFrom
  , distanceFrom#
  , angleFrom
  , distanceAlong
  , distanceLeftOf
  , distanceRightOf
  , placeIn
  , relativeTo
  , on
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
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Length qualified as Length
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis2d (Axis2d)
  , Frame2d (Frame2d)
  , Orientation2d (Orientation2d)
  , Plane3d (Plane3d)
  , PlaneOrientation3d (PlaneOrientation3d)
  , Point2d (Point2d, Position2d)
  , Point3d
  , Transform2d (Transform2d)
  )
import OpenSolid.Qty (Qty (Qty#))
import OpenSolid.Qty qualified as Qty
import {-# SOURCE #-} OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Unboxed.Math
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d

-- | The point with coordinates (0,0).
origin :: Point2d (space @ units)
origin = Position2d Vector2d.zero

{-# INLINE coerce #-}
coerce :: Point2d (space1 @ units1) -> Point2d (space2 @ units2)
coerce (Position2d p) = Position2d (Vector2d.coerce p)

-- | Construct a point along the X axis, with the given X coordinate.
x :: Qty units -> Point2d (space @ units)
x px = Point2d px Qty.zero

-- | Construct a point along the Y axis, with the given Y coordinate.
y :: Qty units -> Point2d (space @ units)
y py = Point2d Qty.zero py

along :: Axis2d (space @ units) -> Qty units -> Point2d (space @ units)
along (Axis2d originPoint direction) distance = originPoint + distance * direction

{-| Construct a point from polar coordinates (radius and angle).

The angle is measured counterclockwise from the positive X axis.
-}
polar :: Qty units -> Angle -> Point2d (space @ units)
polar r theta = Position2d (Vector2d.polar r theta)

apply :: (Float -> Qty units) -> Float -> Float -> Point2d (space @ units)
apply units fx fy = Point2d (units fx) (units fy)

-- | Construct a point from its X and Y coordinates given in meters.
meters :: Float -> Float -> Point2d (space @ Meters)
meters = apply Length.meters

-- | Construct a point from its X and Y coordinates given in centimeters.
centimeters :: Float -> Float -> Point2d (space @ Meters)
centimeters = apply Length.centimeters

-- | Construct a point from its X and Y coordinates given in millimeters.
millimeters :: Float -> Float -> Point2d (space @ Meters)
millimeters = apply Length.millimeters

{-| Construct a point from its X and Y coordinates given in centimeters.

Short form alias for 'centimeters'.
-}
cm :: Float -> Float -> Point2d (space @ Meters)
cm = centimeters

{-| Construct a point from its X and Y coordinates given in millimeters.

Short form alias for 'millimeters'.
-}
mm :: Float -> Float -> Point2d (space @ Meters)
mm = millimeters

-- | Construct a point from its X and Y coordinates given in inches.
inches :: Float -> Float -> Point2d (space @ Meters)
inches = apply Length.inches

-- | Get the X coordinate of a point.
xCoordinate :: Point2d (space @ units) -> Qty units
xCoordinate (Position2d p) = Vector2d.xComponent p

-- | Get the Y coordinate of a point.
yCoordinate :: Point2d (space @ units) -> Qty units
yCoordinate (Position2d p) = Vector2d.yComponent p

-- | Get the X and Y coordinates of a point.
{-# INLINE coordinates #-}
coordinates :: Point2d (space @ units) -> (Qty units, Qty units)
coordinates (Position2d p) = Vector2d.components p

interpolateFrom ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Float ->
  Point2d (space @ units)
interpolateFrom (Position2d p1) (Position2d p2) t = Position2d (Vector2d.interpolateFrom p1 p2 t)

-- | Find the midpoint between two points.
midpoint :: Point2d (space @ units) -> Point2d (space @ units) -> Point2d (space @ units)
midpoint (Position2d p1) (Position2d p2) = Position2d (Vector2d.midpoint p1 p2)

-- | Compute the distance from one point to another.
distanceFrom :: Point2d (space @ units) -> Point2d (space @ units) -> Qty units
distanceFrom p1 p2 = Qty# (distanceFrom# p1 p2)

{-# INLINE distanceFrom# #-}
distanceFrom# :: Point2d (space @ units) -> Point2d (space @ units) -> Double#
distanceFrom# (Point2d (Qty# x1#) (Qty# y1#)) (Point2d (Qty# x2#) (Qty# y2#)) =
  hypot2# (x2# -# x1#) (y2# -# y1#)

angleFrom :: Point2d (space @ units) -> Point2d (space @ units) -> Angle
angleFrom p1 p2 = Vector2d.angle (p2 - p1)

distanceAlong :: Axis2d (space @ units) -> Point2d (space @ units) -> Qty units
distanceAlong (Axis2d originPoint direction) point = direction `dot` (point - originPoint)

distanceLeftOf :: Axis2d (space @ units) -> Point2d (space @ units) -> Qty units
distanceLeftOf (Axis2d originPoint direction) point = direction `cross` (point - originPoint)

distanceRightOf :: Axis2d (space @ units) -> Point2d (space @ units) -> Qty units
distanceRightOf (Axis2d originPoint direction) point = direction `cross` (originPoint - point)

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Point2d (local @ units) ->
  Point2d (global @ units)
placeIn (Frame2d p0 (Orientation2d i j)) (Point2d px py) = p0 + px * i + py * j

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Point2d (global @ units) ->
  Point2d (local @ units)
relativeTo (Frame2d p0 (Orientation2d i j)) p = let d = p - p0 in Point2d (d `dot` i) (d `dot` j)

{-| Convert a 2D point to 3D point by placing it on a plane.

Given a 2D point defined within a plane's coordinate system,
this returns the corresponding 3D point.
-}
on ::
  Plane3d (space @ units) (Defines localSpace) ->
  Point2d (localSpace @ units) ->
  Point3d (space @ units)
on (Plane3d originPoint (PlaneOrientation3d i j)) (Point2d px py) = originPoint + px * i + py * j

convert :: Qty (units2 :/: units1) -> Point2d (space @ units1) -> Point2d (space @ units2)
convert factor (Position2d p) = Position2d (Vector2d.convert factor p)

unconvert :: Qty (units2 :/: units1) -> Point2d (space @ units2) -> Point2d (space @ units1)
unconvert factor (Position2d p) = Position2d (Vector2d.unconvert factor p)

transformBy :: Transform2d tag (space @ units) -> Point2d (space @ units) -> Point2d (space @ units)
transformBy transform point = do
  let (Transform2d p0 vx vy) = transform
  let (px, py) = coordinates point
  p0 + px * vx + py * vy

translateBy :: Vector2d (space @ units) -> Point2d (space @ units) -> Point2d (space @ units)
translateBy = Transform2d.translateByImpl transformBy

translateIn :: Direction2d space -> Qty units -> Point2d (space @ units) -> Point2d (space @ units)
translateIn = Transform2d.translateInImpl transformBy

translateAlong :: Axis2d (space @ units) -> Qty units -> Point2d (space @ units) -> Point2d (space @ units)
translateAlong = Transform2d.translateAlongImpl transformBy

rotateAround :: Point2d (space @ units) -> Angle -> Point2d (space @ units) -> Point2d (space @ units)
rotateAround = Transform2d.rotateAroundImpl transformBy

mirrorAcross :: Axis2d (space @ units) -> Point2d (space @ units) -> Point2d (space @ units)
mirrorAcross = Transform2d.mirrorAcrossImpl transformBy

scaleAbout :: Point2d (space @ units) -> Float -> Point2d (space @ units) -> Point2d (space @ units)
scaleAbout = Transform2d.scaleAboutImpl transformBy

scaleAlong :: Axis2d (space @ units) -> Float -> Point2d (space @ units) -> Point2d (space @ units)
scaleAlong = Transform2d.scaleAlongImpl transformBy
