{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.LineSegment2d
  ( LineSegment2d (LineSegment2d)
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

import OpenSolid.Bounded2d (Bounded2d)
import OpenSolid.Bounded2d qualified as Bounded2d
import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Unboxed.Math

-- | A line segment in 2D, with a start point and end point.
data LineSegment2d units space
  = -- | Construct a line segment from its start and end points.
    LineSegment2d
    { startPoint :: Point2d units space
    , endPoint :: Point2d units space
    }

instance FFI (LineSegment2d Meters FFI.Space) where
  representation = FFI.classRepresentation "LineSegment2d"

instance Bounded2d (LineSegment2d units space) units space where
  bounds = bounds

-- | Get the start point of a line segment.
{-# INLINE startPoint #-}
startPoint :: LineSegment2d units space -> Point2d units space
startPoint = (.startPoint)

-- | Get the end point of a line segment.
{-# INLINE endPoint #-}
endPoint :: LineSegment2d units space -> Point2d units space
endPoint = (.endPoint)

-- | Get the start and end points of a line segment as a tuple.
endpoints :: LineSegment2d units space -> (Point2d units space, Point2d units space)
endpoints (LineSegment2d p1 p2) = (p1, p2)

-- | Get the length of a line segment.
length :: LineSegment2d units space -> Quantity units
length segment = Quantity# (length# segment)

{-# INLINE length# #-}
length# :: LineSegment2d units space -> Double#
length# (LineSegment2d p1 p2) = Point2d.distanceFrom# p1 p2

bounds :: LineSegment2d units space -> Bounds2d units space
bounds (LineSegment2d p1 p2) = Bounds2d.hull2 p1 p2

{-| Get the distance from a line segment to a point.

This is measured from the point on the line segment closest to the given point
(note that the closest point might be within the line segment,
or might be one of the line segment's endpoints,
so this is not necessarily a *perpendicular* distance).
-}
distanceTo :: Point2d units space -> LineSegment2d units space -> Quantity units
distanceTo p0 segment = Quantity# (distanceTo# p0 segment)

{-# INLINEABLE distanceTo# #-}
distanceTo# :: Point2d units space -> LineSegment2d units space -> Double#
distanceTo# p0 (LineSegment2d p1 p2) = do
  let !(Point2d (Quantity# x0#) (Quantity# y0#)) = p0
  let !(Point2d (Quantity# x1#) (Quantity# y1#)) = p1
  let !(Point2d (Quantity# x2#) (Quantity# y2#)) = p2
  let ux# = x0# -# x1#
  let uy# = y0# -# y1#
  let vx# = x2# -# x1#
  let vy# = y2# -# y1#
  let lengthSquared# = vx# *# vx# +# vy# *# vy#
  let dotProduct# = ux# *# vx# +# uy# *# vy#
  case (# dotProduct# <=# 0.0##, dotProduct# >=# lengthSquared# #) of
    (# 1#, _ #) -> hypot2# ux# uy#
    (# _, 1# #) -> hypot2# (x0# -# x2#) (y0# -# y2#)
    (# _, _ #) -> abs# (vx# *# uy# -# vy# *# ux#) /# sqrt# lengthSquared#
