module OpenSolid.Polymorphic.Point2d
  ( Point2d (Point2d)
  , origin
  , coerce
  , x
  , y
  , along
  , polar
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
  , placeOn
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
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Quantity qualified as Quantity
import {-# SOURCE #-} OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Unboxed.Math
import OpenSolid.Polymorphic.Vector2d (Vector2d)
import OpenSolid.Polymorphic.Vector2d qualified as Vector2d

-- | The point with coordinates (0,0).
origin :: Point2d units space
origin = Position2d Vector2d.zero

{-# INLINE coerce #-}
coerce :: Point2d units1 space1 -> Point2d units2 space2
coerce (Position2d p) = Position2d (Vector2d.coerce p)

-- | Construct a point along the X axis, with the given X coordinate.
x :: Quantity units -> Point2d units space
x px = Point2d px Quantity.zero

-- | Construct a point along the Y axis, with the given Y coordinate.
y :: Quantity units -> Point2d units space
y py = Point2d Quantity.zero py

along :: Axis2d units space -> Quantity units -> Point2d units space
along (Axis2d originPoint direction) distance = originPoint .+. distance .*. direction

{-| Construct a point from polar coordinates (radius and angle).

The angle is measured counterclockwise from the positive X axis.
-}
polar :: Quantity units -> Angle -> Point2d units space
polar r theta = Position2d (Vector2d.polar r theta)

-- | Get the X coordinate of a point.
{-# INLINE xCoordinate #-}
xCoordinate :: Point2d units space -> Quantity units
xCoordinate (Position2d p) = Vector2d.xComponent p

-- | Get the Y coordinate of a point.
{-# INLINE yCoordinate #-}
yCoordinate :: Point2d units space -> Quantity units
yCoordinate (Position2d p) = Vector2d.yComponent p

-- | Get the X and Y coordinates of a point.
{-# INLINE coordinates #-}
coordinates :: Point2d units space -> (Quantity units, Quantity units)
coordinates (Position2d p) = Vector2d.components p

interpolateFrom ::
  Point2d units space ->
  Point2d units space ->
  Number ->
  Point2d units space
interpolateFrom (Position2d p1) (Position2d p2) t = Position2d (Vector2d.interpolateFrom p1 p2 t)

-- | Find the midpoint between two points.
midpoint :: Point2d units space -> Point2d units space -> Point2d units space
midpoint (Position2d p1) (Position2d p2) = Position2d (Vector2d.midpoint p1 p2)

-- | Compute the distance from one point to another.
distanceFrom :: Point2d units space -> Point2d units space -> Quantity units
distanceFrom p1 p2 = Quantity# (distanceFrom# p1 p2)

{-# INLINE distanceFrom# #-}
distanceFrom# :: Point2d units space -> Point2d units space -> Double#
distanceFrom# (Point2d (Quantity# x1#) (Quantity# y1#)) (Point2d (Quantity# x2#) (Quantity# y2#)) =
  hypot2# (x2# -# x1#) (y2# -# y1#)

angleFrom :: Point2d units space -> Point2d units space -> Angle
angleFrom p1 p2 = Vector2d.angle (p2 .-. p1)

distanceAlong :: Axis2d units space -> Point2d units space -> Quantity units
distanceAlong (Axis2d originPoint direction) point = direction `dot` (point .-. originPoint)

distanceLeftOf :: Axis2d units space -> Point2d units space -> Quantity units
distanceLeftOf (Axis2d originPoint direction) point = direction `cross` (point .-. originPoint)

distanceRightOf :: Axis2d units space -> Point2d units space -> Quantity units
distanceRightOf (Axis2d originPoint direction) point = direction `cross` (originPoint .-. point)

placeIn :: Frame2d units global local -> Point2d units local -> Point2d units global
placeIn (Frame2d p0 (Orientation2d i j)) (Point2d px py) = p0 .+. px .*. i .+. py .*. j

relativeTo :: Frame2d units global local -> Point2d units global -> Point2d units local
relativeTo (Frame2d p0 (Orientation2d i j)) p = let d = p .-. p0 in Point2d (d `dot` i) (d `dot` j)

{-| Convert a 2D point to 3D point by placing it on a plane.

Given a 2D point defined within a plane's coordinate system,
this returns the corresponding 3D point.
-}
placeOn :: Plane3d global local -> Point2d Meters local -> Point3d global
placeOn (Plane3d originPoint (PlaneOrientation3d i j)) (Point2d px py) =
  originPoint .+. px .*. i .+. py .*. j

convert :: Quantity (units2 ?/? units1) -> Point2d units1 space -> Point2d units2 space
convert factor (Position2d p) = Position2d (Vector2d.convert factor p)

unconvert :: Quantity (units2 ?/? units1) -> Point2d units2 space -> Point2d units1 space
unconvert factor (Position2d p) = Position2d (Vector2d.unconvert factor p)

transformBy :: Transform2d tag units space -> Point2d units space -> Point2d units space
transformBy transform point = do
  let (Transform2d p0 vx vy) = transform
  let (px, py) = coordinates point
  p0 .+. px .*. vx .+. py .*. vy

translateBy ::
  Vector2d units space ->
  Point2d units space ->
  Point2d units space
translateBy = Transform2d.translateByImpl transformBy

translateIn ::
  Direction2d space ->
  Quantity units ->
  Point2d units space ->
  Point2d units space
translateIn = Transform2d.translateInImpl transformBy

translateAlong ::
  Axis2d units space ->
  Quantity units ->
  Point2d units space ->
  Point2d units space
translateAlong = Transform2d.translateAlongImpl transformBy

rotateAround ::
  Point2d units space ->
  Angle ->
  Point2d units space ->
  Point2d units space
rotateAround = Transform2d.rotateAroundImpl transformBy

mirrorAcross ::
  Axis2d units space ->
  Point2d units space ->
  Point2d units space
mirrorAcross = Transform2d.mirrorAcrossImpl transformBy

scaleAbout ::
  Point2d units space ->
  Number ->
  Point2d units space ->
  Point2d units space
scaleAbout = Transform2d.scaleAboutImpl transformBy

scaleAlong ::
  Axis2d units space ->
  Number ->
  Point2d units space ->
  Point2d units space
scaleAlong = Transform2d.scaleAlongImpl transformBy
