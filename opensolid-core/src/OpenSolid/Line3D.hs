module OpenSolid.Line3D
  ( Line3D (Line3D)
  , startPoint
  , endPoint
  , endpoints
  , length
  , length#
  , bounds
  , distanceTo
  , distanceTo#
  )
where

import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Length (Length)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Primitives (Point3D (Point3D))
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Unboxed.Math

-- | A line in 3D, with a start point and end point.
data Line3D space
  = -- | Construct a line from its start and end points.
    Line3D (Point3D space) (Point3D space)

instance FFI (Line3D FFI.Space) where
  representation = FFI.classRepresentation "Line3D"

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
