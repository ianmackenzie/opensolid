module OpenSolid.Line3D
  ( Line3D (Line3D)
  , on
  , startPoint
  , endPoint
  , endpoints
  , length
  , length#
  , bounds
  , distanceTo
  , distanceTo#
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
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Length (Length)
import OpenSolid.Line2D (Line2D (Line2D))
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Primitives (Point3D (Point3D))
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Transform3D qualified as Transform3D
import OpenSolid.Unboxed.Math
import OpenSolid.Vector3D (Vector3D)

-- | A line in 3D, with a start point and end point.
data Line3D space
  = -- | Construct a line from its start and end points.
    Line3D (Point3D space) (Point3D space)

instance FFI (Line3D FFI.Space) where
  representation = FFI.classRepresentation "Line3D"

on :: Plane3D space local -> Line2D Meters local -> Line3D space
on plane (Line2D p1 p2) = Line3D (Point3D.on plane p1) (Point3D.on plane p2)

-- | Get the start point of a line.
{-# INLINE startPoint #-}
startPoint :: Line3D space -> Point3D space
startPoint (Line3D p1 _) = p1

-- | Get the end point of a line.
{-# INLINE endPoint #-}
endPoint :: Line3D space -> Point3D space
endPoint (Line3D _ p2) = p2

-- | Get the start and end points of a line as a tuple.
endpoints :: Line3D space -> (Point3D space, Point3D space)
endpoints (Line3D p1 p2) = (p1, p2)

-- | Get the length of a line.
length :: Line3D space -> Length
length line = Quantity# (length# line)

{-# INLINE length# #-}
length# :: Line3D space -> Double#
length# (Line3D p1 p2) = Point3D.distanceFrom# p1 p2

bounds :: Line3D space -> Bounds3D space
bounds (Line3D p1 p2) = Bounds3D.hull2 p1 p2

{-| Get the distance from a line to a point.

This is measured from the point on the line closest to the given point
(note that the closest point might be within the line,
or might be one of the line's endpoints,
so this is not necessarily a *perpendicular* distance).
-}
distanceTo :: Point3D space -> Line3D space -> Length
distanceTo p0 line = Quantity# (distanceTo# p0 line)

{-# INLINEABLE distanceTo# #-}
distanceTo# :: Point3D space -> Line3D space -> Double#
distanceTo# p0 (Line3D p1 p2) = do
  let !(Point3D (Quantity# x0#) (Quantity# y0#) (Quantity# z0#)) = p0
  let !(Point3D (Quantity# x1#) (Quantity# y1#) (Quantity# z1#)) = p1
  let !(Point3D (Quantity# x2#) (Quantity# y2#) (Quantity# z2#)) = p2
  let ux# = x0# -# x1#
  let uy# = y0# -# y1#
  let uz# = z0# -# z1#
  let vx# = x2# -# x1#
  let vy# = y2# -# y1#
  let vz# = z2# -# z1#
  let lengthSquared# = vx# *# vx# +# vy# *# vy# +# vz# *# vz#
  let dotProduct# = ux# *# vx# +# uy# *# vy# +# uz# *# vz#
  case dotProduct# <=# 0.0## of
    1# -> hypot3# ux# uy# uz#
    _ -> case dotProduct# >=# lengthSquared# of
      1# -> hypot3# (x0# -# x2#) (y0# -# y2#) (z0# -# z2#)
      _ -> do
        let crossX# = vy# *# uz# -# vz# *# uy#
        let crossY# = vz# *# ux# -# vx# *# uz#
        let crossZ# = vx# *# uy# -# vy# *# ux#
        hypot3# crossX# crossY# crossZ# /# sqrt# lengthSquared#

reverse :: Line3D space -> Line3D space
reverse (Line3D p1 p2) = Line3D p2 p1

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
