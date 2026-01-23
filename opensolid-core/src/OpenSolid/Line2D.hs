{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Line2D
  ( Line2D (Line2D)
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

import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Unboxed.Math

-- | A line in 2D, with a start point and end point.
data Line2D units space
  = -- | Construct a line from its start and end points.
    Line2D
    { startPoint :: Point2D units space
    , endPoint :: Point2D units space
    }

instance FFI (Line2D Meters FFI.Space) where
  representation = FFI.classRepresentation "Line2D"

instance FFI (Line2D Unitless UvSpace) where
  representation = FFI.classRepresentation "UvLine"

-- | Get the start point of a line.
{-# INLINE startPoint #-}
startPoint :: Line2D units space -> Point2D units space
startPoint = (.startPoint)

-- | Get the end point of a line.
{-# INLINE endPoint #-}
endPoint :: Line2D units space -> Point2D units space
endPoint = (.endPoint)

-- | Get the start and end points of a line as a tuple.
endpoints :: Line2D units space -> (Point2D units space, Point2D units space)
endpoints (Line2D p1 p2) = (p1, p2)

-- | Get the length of a line.
length :: Line2D units space -> Quantity units
length line = Quantity# (length# line)

{-# INLINE length# #-}
length# :: Line2D units space -> Double#
length# (Line2D p1 p2) = Point2D.distanceFrom# p1 p2

bounds :: Line2D units space -> Bounds2D units space
bounds (Line2D p1 p2) = Bounds2D.hull2 p1 p2

{-| Get the distance from a line to a point.

This is measured from the point on the line closest to the given point
(note that the closest point might be within the line,
or might be one of the line's endpoints,
so this is not necessarily a *perpendicular* distance).
-}
distanceTo :: Point2D units space -> Line2D units space -> Quantity units
distanceTo p0 line = Quantity# (distanceTo# p0 line)

{-# INLINEABLE distanceTo# #-}
distanceTo# :: Point2D units space -> Line2D units space -> Double#
distanceTo# p0 (Line2D p1 p2) = do
  let !(Point2D (Quantity# x0#) (Quantity# y0#)) = p0
  let !(Point2D (Quantity# x1#) (Quantity# y1#)) = p1
  let !(Point2D (Quantity# x2#) (Quantity# y2#)) = p2
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
