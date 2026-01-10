module OpenSolid.Point2D
  ( Point2D (Point2D)
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
import OpenSolid.Length qualified as Length
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis2d (Axis2d)
  , Frame2d (Frame2d)
  , Orientation2d (Orientation2d)
  , Plane3d (Plane3d)
  , PlaneOrientation3d (PlaneOrientation3d)
  , Point2D (Point2D, Position2D)
  , Point3d
  , Transform2d (Transform2d)
  )
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Quantity qualified as Quantity
import {-# SOURCE #-} OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Unboxed.Math
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector2D qualified as Vector2D

-- | The point with coordinates (0,0).
origin :: Point2D units space
origin = Position2D Vector2D.zero

{-# INLINE coerce #-}
coerce :: Point2D units1 space1 -> Point2D units2 space2
coerce (Position2D p) = Position2D (Vector2D.coerce p)

-- | Construct a point along the X axis, with the given X coordinate.
x :: Quantity units -> Point2D units space
x px = Point2D px Quantity.zero

-- | Construct a point along the Y axis, with the given Y coordinate.
y :: Quantity units -> Point2D units space
y py = Point2D Quantity.zero py

along :: Axis2d units space -> Quantity units -> Point2D units space
along (Axis2d originPoint direction) distance = originPoint .+. distance .*. direction

{-| Construct a point from polar coordinates (radius and angle).

The angle is measured counterclockwise from the positive X axis.
-}
polar :: Quantity units -> Angle -> Point2D units space
polar r theta = Position2D (Vector2D.polar r theta)

apply :: (Number -> Quantity units) -> Number -> Number -> Point2D units space
apply units fx fy = Point2D (units fx) (units fy)

-- | Construct a point from its X and Y coordinates given in meters.
meters :: Number -> Number -> Point2D Meters space
meters = apply Length.meters

-- | Construct a point from its X and Y coordinates given in centimeters.
centimeters :: Number -> Number -> Point2D Meters space
centimeters = apply Length.centimeters

-- | Construct a point from its X and Y coordinates given in millimeters.
millimeters :: Number -> Number -> Point2D Meters space
millimeters = apply Length.millimeters

{-| Construct a point from its X and Y coordinates given in centimeters.

Short form alias for 'centimeters'.
-}
cm :: Number -> Number -> Point2D Meters space
cm = centimeters

{-| Construct a point from its X and Y coordinates given in millimeters.

Short form alias for 'millimeters'.
-}
mm :: Number -> Number -> Point2D Meters space
mm = millimeters

-- | Construct a point from its X and Y coordinates given in inches.
inches :: Number -> Number -> Point2D Meters space
inches = apply Length.inches

-- | Get the X coordinate of a point.
{-# INLINE xCoordinate #-}
xCoordinate :: Point2D units space -> Quantity units
xCoordinate (Position2D p) = Vector2D.xComponent p

-- | Get the Y coordinate of a point.
{-# INLINE yCoordinate #-}
yCoordinate :: Point2D units space -> Quantity units
yCoordinate (Position2D p) = Vector2D.yComponent p

-- | Get the X and Y coordinates of a point.
{-# INLINE coordinates #-}
coordinates :: Point2D units space -> (Quantity units, Quantity units)
coordinates (Position2D p) = Vector2D.components p

interpolateFrom ::
  Point2D units space ->
  Point2D units space ->
  Number ->
  Point2D units space
interpolateFrom (Position2D p1) (Position2D p2) t = Position2D (Vector2D.interpolateFrom p1 p2 t)

-- | Find the midpoint between two points.
midpoint :: Point2D units space -> Point2D units space -> Point2D units space
midpoint (Position2D p1) (Position2D p2) = Position2D (Vector2D.midpoint p1 p2)

-- | Compute the distance from one point to another.
distanceFrom :: Point2D units space -> Point2D units space -> Quantity units
distanceFrom p1 p2 = Quantity# (distanceFrom# p1 p2)

{-# INLINE distanceFrom# #-}
distanceFrom# :: Point2D units space -> Point2D units space -> Double#
distanceFrom# (Point2D (Quantity# x1#) (Quantity# y1#)) (Point2D (Quantity# x2#) (Quantity# y2#)) =
  hypot2# (x2# -# x1#) (y2# -# y1#)

angleFrom :: Point2D units space -> Point2D units space -> Angle
angleFrom p1 p2 = Vector2D.angle (p2 .-. p1)

distanceAlong :: Axis2d units space -> Point2D units space -> Quantity units
distanceAlong (Axis2d originPoint direction) point = direction `dot` (point .-. originPoint)

distanceLeftOf :: Axis2d units space -> Point2D units space -> Quantity units
distanceLeftOf (Axis2d originPoint direction) point = direction `cross` (point .-. originPoint)

distanceRightOf :: Axis2d units space -> Point2D units space -> Quantity units
distanceRightOf (Axis2d originPoint direction) point = direction `cross` (originPoint .-. point)

placeIn :: Frame2d units global local -> Point2D units local -> Point2D units global
placeIn (Frame2d p0 (Orientation2d i j)) (Point2D px py) = p0 .+. px .*. i .+. py .*. j

relativeTo :: Frame2d units global local -> Point2D units global -> Point2D units local
relativeTo (Frame2d p0 (Orientation2d i j)) p = let d = p .-. p0 in Point2D (d `dot` i) (d `dot` j)

{-| Convert a 2D point to 3D point by placing it on a plane.

Given a 2D point defined within a plane's coordinate system,
this returns the corresponding 3D point.
-}
placeOn :: Plane3d global local -> Point2D Meters local -> Point3d global
placeOn (Plane3d originPoint (PlaneOrientation3d i j)) (Point2D px py) =
  originPoint .+. px .*. i .+. py .*. j

convert :: Quantity (units2 ?/? units1) -> Point2D units1 space -> Point2D units2 space
convert factor (Position2D p) = Position2D (Vector2D.convert factor p)

unconvert :: Quantity (units2 ?/? units1) -> Point2D units2 space -> Point2D units1 space
unconvert factor (Position2D p) = Position2D (Vector2D.unconvert factor p)

transformBy :: Transform2d tag units space -> Point2D units space -> Point2D units space
transformBy transform point = do
  let (Transform2d p0 vx vy) = transform
  let (px, py) = coordinates point
  p0 .+. px .*. vx .+. py .*. vy

translateBy ::
  Vector2D units space ->
  Point2D units space ->
  Point2D units space
translateBy = Transform2d.translateByImpl transformBy

translateIn ::
  Direction2d space ->
  Quantity units ->
  Point2D units space ->
  Point2D units space
translateIn = Transform2d.translateInImpl transformBy

translateAlong ::
  Axis2d units space ->
  Quantity units ->
  Point2D units space ->
  Point2D units space
translateAlong = Transform2d.translateAlongImpl transformBy

rotateAround ::
  Point2D units space ->
  Angle ->
  Point2D units space ->
  Point2D units space
rotateAround = Transform2d.rotateAroundImpl transformBy

mirrorAcross ::
  Axis2d units space ->
  Point2D units space ->
  Point2D units space
mirrorAcross = Transform2d.mirrorAcrossImpl transformBy

scaleAbout ::
  Point2D units space ->
  Number ->
  Point2D units space ->
  Point2D units space
scaleAbout = Transform2d.scaleAboutImpl transformBy

scaleAlong ::
  Axis2d units space ->
  Number ->
  Point2D units space ->
  Point2D units space
scaleAlong = Transform2d.scaleAlongImpl transformBy
