module OpenSolid.Point2D
  ( Point2D
  , pattern Point2D
  , origin
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
  , angleFrom
  , distanceAlong
  , distanceLeftOf
  , distanceRightOf
  , placeIn
  , relativeTo
  , placeOn
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
import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Primitives (Plane3d, Point3d)
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Vector2d (Vector2d)

type Point2D space = Point2d Meters space

{-# COMPLETE Point2D #-}

-- | Construct a point from its X and Y coordinates.
{-# INLINE Point2D #-}
pattern Point2D :: Length -> Length -> Point2D space
pattern Point2D px py = Point2d px py

-- | The point with coordinates (0,0).
origin :: Point2D space
origin = Point2d.origin

-- | Construct a point along the X axis, with the given X coordinate.
x :: Length -> Point2D space
x = Point2d.x

-- | Construct a point along the Y axis, with the given Y coordinate.
y :: Length -> Point2D space
y = Point2d.y

along :: Axis2d Meters space -> Length -> Point2D space
along = Point2d.along

{-| Construct a point from polar coordinates (radius and angle).

The angle is measured counterclockwise from the positive X axis.
-}
polar :: Length -> Angle -> Point2D space
polar = Point2d.polar

apply :: (Number -> Length) -> Number -> Number -> Point2D space
apply units fx fy = Point2D (units fx) (units fy)

-- | Construct a point from its X and Y coordinates given in meters.
meters :: Number -> Number -> Point2D space
meters = apply Length.meters

-- | Construct a point from its X and Y coordinates given in centimeters.
centimeters :: Number -> Number -> Point2D space
centimeters = apply Length.centimeters

-- | Construct a point from its X and Y coordinates given in millimeters.
millimeters :: Number -> Number -> Point2D space
millimeters = apply Length.millimeters

{-| Construct a point from its X and Y coordinates given in centimeters.

Short form alias for 'centimeters'.
-}
cm :: Number -> Number -> Point2D space
cm = centimeters

{-| Construct a point from its X and Y coordinates given in millimeters.

Short form alias for 'millimeters'.
-}
mm :: Number -> Number -> Point2D space
mm = millimeters

-- | Construct a point from its X and Y coordinates given in inches.
inches :: Number -> Number -> Point2D space
inches = apply Length.inches

-- | Get the X coordinate of a point.
{-# INLINE xCoordinate #-}
xCoordinate :: Point2D space -> Length
xCoordinate = Point2d.xCoordinate

-- | Get the Y coordinate of a point.
{-# INLINE yCoordinate #-}
yCoordinate :: Point2D space -> Length
yCoordinate = Point2d.yCoordinate

-- | Get the X and Y coordinates of a point.
{-# INLINE coordinates #-}
coordinates :: Point2D space -> (Length, Length)
coordinates = Point2d.coordinates

interpolateFrom :: Point2D space -> Point2D space -> Number -> Point2D space
interpolateFrom = Point2d.interpolateFrom

-- | Find the midpoint between two points.
midpoint :: Point2D space -> Point2D space -> Point2D space
midpoint = Point2d.midpoint

-- | Compute the distance from one point to another.
distanceFrom :: Point2D space -> Point2D space -> Length
distanceFrom = Point2d.distanceFrom

angleFrom :: Point2D space -> Point2D space -> Angle
angleFrom = Point2d.angleFrom

distanceAlong :: Axis2d Meters space -> Point2D space -> Length
distanceAlong = Point2d.distanceAlong

distanceLeftOf :: Axis2d Meters space -> Point2D space -> Length
distanceLeftOf = Point2d.distanceLeftOf

distanceRightOf :: Axis2d Meters space -> Point2D space -> Length
distanceRightOf = Point2d.distanceRightOf

placeIn :: Frame2d Meters global local -> Point2D local -> Point2D global
placeIn = Point2d.placeIn

relativeTo :: Frame2d Meters global local -> Point2D global -> Point2D local
relativeTo = Point2d.relativeTo

{-| Convert a 2D point to 3D point by placing it on a plane.

Given a 2D point defined within a plane's coordinate system,
this returns the corresponding 3D point.
-}
placeOn :: Plane3d global local -> Point2D local -> Point3d global
placeOn = Point2d.placeOn

transformBy :: Transform2d tag Meters space -> Point2D space -> Point2D space
transformBy = Point2d.transformBy

translateBy :: Vector2d Meters space -> Point2D space -> Point2D space
translateBy = Point2d.translateBy

translateIn :: Direction2d space -> Length -> Point2D space -> Point2D space
translateIn = Point2d.translateIn

translateAlong :: Axis2d Meters space -> Length -> Point2D space -> Point2D space
translateAlong = Point2d.translateAlong

rotateAround :: Point2D space -> Angle -> Point2D space -> Point2D space
rotateAround = Point2d.rotateAround

mirrorAcross :: Axis2d Meters space -> Point2D space -> Point2D space
mirrorAcross = Point2d.mirrorAcross

scaleAbout :: Point2D space -> Number -> Point2D space -> Point2D space
scaleAbout = Point2d.scaleAbout

scaleAlong :: Axis2d Meters space -> Number -> Point2D space -> Point2D space
scaleAlong = Point2d.scaleAlong
