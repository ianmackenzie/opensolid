module OpenSolid.Line3D
  ( Line3D
  , pattern Line3D
  , on
  , startPoint
  , endPoint
  , endpoints
  , length
  , bounds
  , distanceTo
  , reverse
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
import OpenSolid.Axis3D (Axis3D)
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Length (Length)
import OpenSolid.Line (Line (Line))
import OpenSolid.Line qualified as Line
import OpenSolid.Line2D (Line2D, pattern Line2D)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Primitives (Point3D)
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Transform3D qualified as Transform3D
import OpenSolid.Vector3D (Vector3D)

-- | A line in 3D, with a start point and end point.
type Line3D space =
  Line 3 Meters space

-- | Construct a line from its start and end points.
{-# INLINE Line3D #-}
pattern Line3D :: Point3D space -> Point3D space -> Line3D space
pattern Line3D p1 p2 = Line p1 p2

{-# COMPLETE Line3D #-}

on :: Plane3D space local -> Line2D Meters local -> Line3D space
on plane (Line2D p1 p2) = Line3D (Point3D.on plane p1) (Point3D.on plane p2)

-- | Get the start point of a line.
{-# INLINE startPoint #-}
startPoint :: Line3D space -> Point3D space
startPoint = Line.startPoint

-- | Get the end point of a line.
{-# INLINE endPoint #-}
endPoint :: Line3D space -> Point3D space
endPoint = Line.endPoint

-- | Get the start and end points of a line as a tuple.
endpoints :: Line3D space -> (Point3D space, Point3D space)
endpoints = Line.endpoints

-- | Get the length of a line.
length :: Line3D space -> Length
length = Line.length

bounds :: Line3D space -> Bounds3D space
bounds = Line.bounds

{-| Get the distance from a line to a point.

This is measured from the point on the line closest to the given point
(note that the closest point might be within the line,
or might be one of the line's endpoints,
so this is not necessarily a *perpendicular* distance).
-}
distanceTo :: Point3D space -> Line3D space -> Length
distanceTo = Line.distanceTo

reverse :: Line3D space -> Line3D space
reverse = Line.reverse

transformBy :: Transform3D tag space -> Line3D space -> Line3D space
transformBy transform (Line3D p1 p2) =
  Line3D (Point3D.transformBy transform p1) (Point3D.transformBy transform p2)

translateBy :: Vector3D Meters space -> Line3D space -> Line3D space
translateBy = Transform3D.translateByImpl transformBy

translateIn :: Direction3D space -> Length -> Line3D space -> Line3D space
translateIn = Transform3D.translateInImpl transformBy

translateAlong :: Axis3D space -> Length -> Line3D space -> Line3D space
translateAlong = Transform3D.translateAlongImpl transformBy

rotateAround :: Axis3D space -> Angle -> Line3D space -> Line3D space
rotateAround = Transform3D.rotateAroundImpl transformBy

mirrorAcross :: Plane3D space local -> Line3D space -> Line3D space
mirrorAcross = Transform3D.mirrorAcrossImpl transformBy

scaleAbout :: Point3D space -> Number -> Line3D space -> Line3D space
scaleAbout = Transform3D.scaleAboutImpl transformBy

scaleAlong :: Axis3D space -> Number -> Line3D space -> Line3D space
scaleAlong = Transform3D.scaleAlongImpl transformBy
