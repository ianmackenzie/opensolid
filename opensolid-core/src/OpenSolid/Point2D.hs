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
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Length qualified as Length
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis2D (Axis2D)
  , Frame2D (Frame2D)
  , Orientation2D (Orientation2D)
  , Plane3D (Plane3D)
  , PlaneOrientation3D (PlaneOrientation3D)
  , Point2D (Point2D, Position2D)
  , Point3D
  , Transform2D (Transform2D)
  )
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Quantity qualified as Quantity
import {-# SOURCE #-} OpenSolid.Transform2D qualified as Transform2D
import OpenSolid.Unboxed.Math
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector2D qualified as Vector2D

-- | The point with coordinates (0,0).
origin :: Point2D units
origin = Position2D Vector2D.zero

{-# INLINE coerce #-}
coerce :: Point2D units1 -> Point2D units2
coerce (Position2D p) = Position2D (Vector2D.coerce p)

-- | Construct a point along the X axis, with the given X coordinate.
x :: Quantity units -> Point2D units
x px = Point2D px Quantity.zero

-- | Construct a point along the Y axis, with the given Y coordinate.
y :: Quantity units -> Point2D units
y py = Point2D Quantity.zero py

along :: Axis2D units -> Quantity units -> Point2D units
along (Axis2D originPoint direction) distance = originPoint + distance * direction

{-| Construct a point from polar coordinates (radius and angle).

The angle is measured counterclockwise from the positive X axis.
-}
polar :: Quantity units -> Angle -> Point2D units
polar r theta = Position2D (Vector2D.polar r theta)

apply :: (Number -> Quantity units) -> Number -> Number -> Point2D units
apply units fx fy = Point2D (units fx) (units fy)

-- | Construct a point from its X and Y coordinates given in meters.
meters :: Number -> Number -> Point2D Meters
meters = apply Length.meters

-- | Construct a point from its X and Y coordinates given in centimeters.
centimeters :: Number -> Number -> Point2D Meters
centimeters = apply Length.centimeters

-- | Construct a point from its X and Y coordinates given in millimeters.
millimeters :: Number -> Number -> Point2D Meters
millimeters = apply Length.millimeters

{-| Construct a point from its X and Y coordinates given in centimeters.

Short form alias for 'centimeters'.
-}
cm :: Number -> Number -> Point2D Meters
cm = centimeters

{-| Construct a point from its X and Y coordinates given in millimeters.

Short form alias for 'millimeters'.
-}
mm :: Number -> Number -> Point2D Meters
mm = millimeters

-- | Construct a point from its X and Y coordinates given in inches.
inches :: Number -> Number -> Point2D Meters
inches = apply Length.inches

-- | Get the X coordinate of a point.
{-# INLINE xCoordinate #-}
xCoordinate :: Point2D units -> Quantity units
xCoordinate (Position2D p) = Vector2D.xComponent p

-- | Get the Y coordinate of a point.
{-# INLINE yCoordinate #-}
yCoordinate :: Point2D units -> Quantity units
yCoordinate (Position2D p) = Vector2D.yComponent p

-- | Get the X and Y coordinates of a point.
{-# INLINE coordinates #-}
coordinates :: Point2D units -> (Quantity units, Quantity units)
coordinates (Position2D p) = Vector2D.components p

interpolateFrom ::
  Point2D units ->
  Point2D units ->
  Number ->
  Point2D units
interpolateFrom (Position2D p1) (Position2D p2) t = Position2D (Vector2D.interpolateFrom p1 p2 t)

-- | Find the midpoint between two points.
midpoint :: Point2D units -> Point2D units -> Point2D units
midpoint (Position2D p1) (Position2D p2) = Position2D (Vector2D.midpoint p1 p2)

-- | Compute the distance from one point to another.
distanceFrom :: Point2D units -> Point2D units -> Quantity units
distanceFrom p1 p2 = Quantity# (distanceFrom# p1 p2)

{-# INLINE distanceFrom# #-}
distanceFrom# :: Point2D units -> Point2D units -> Double#
distanceFrom# (Point2D (Quantity# x1#) (Quantity# y1#)) (Point2D (Quantity# x2#) (Quantity# y2#)) =
  hypot2# (x2# -# x1#) (y2# -# y1#)

angleFrom :: Point2D units -> Point2D units -> Angle
angleFrom p1 p2 = Vector2D.angle (p2 - p1)

distanceAlong :: Axis2D units -> Point2D units -> Quantity units
distanceAlong (Axis2D originPoint direction) point = direction `dot` (point - originPoint)

distanceLeftOf :: Axis2D units -> Point2D units -> Quantity units
distanceLeftOf (Axis2D originPoint direction) point = direction `cross` (point - originPoint)

distanceRightOf :: Axis2D units -> Point2D units -> Quantity units
distanceRightOf (Axis2D originPoint direction) point = direction `cross` (originPoint - point)

placeIn :: Frame2D units -> Point2D units -> Point2D units
placeIn (Frame2D p0 (Orientation2D i j)) (Point2D px py) = p0 + px * i + py * j

relativeTo :: Frame2D units -> Point2D units -> Point2D units
relativeTo (Frame2D p0 (Orientation2D i j)) p = let d = p - p0 in Point2D (d `dot` i) (d `dot` j)

{-| Convert a 2D point to 3D point by placing it on a plane.

Given a 2D point defined within a plane's coordinate system,
this returns the corresponding 3D point.
-}
placeOn :: Plane3D space -> Point2D Meters -> Point3D space
placeOn (Plane3D originPoint (PlaneOrientation3D i j)) (Point2D px py) =
  originPoint + px * i + py * j

convert :: Quantity (units2 ?/? units1) -> Point2D units1 -> Point2D units2
convert factor (Position2D p) = Position2D (Vector2D.convert factor p)

unconvert :: Quantity (units2 ?/? units1) -> Point2D units2 -> Point2D units1
unconvert factor (Position2D p) = Position2D (Vector2D.unconvert factor p)

transformBy :: Transform2D tag units -> Point2D units -> Point2D units
transformBy transform point = do
  let (Transform2D p0 vx vy) = transform
  let (px, py) = coordinates point
  p0 + px * vx + py * vy

translateBy :: Vector2D units -> Point2D units -> Point2D units
translateBy = Transform2D.translateByImpl transformBy

translateIn :: Direction2D -> Quantity units -> Point2D units -> Point2D units
translateIn = Transform2D.translateInImpl transformBy

translateAlong :: Axis2D units -> Quantity units -> Point2D units -> Point2D units
translateAlong = Transform2D.translateAlongImpl transformBy

rotateAround :: Point2D units -> Angle -> Point2D units -> Point2D units
rotateAround = Transform2D.rotateAroundImpl transformBy

mirrorAcross :: Axis2D units -> Point2D units -> Point2D units
mirrorAcross = Transform2D.mirrorAcrossImpl transformBy

scaleAbout :: Point2D units -> Number -> Point2D units -> Point2D units
scaleAbout = Transform2D.scaleAboutImpl transformBy

scaleAlong :: Axis2D units -> Number -> Point2D units -> Point2D units
scaleAlong = Transform2D.scaleAlongImpl transformBy
