module OpenSolid.Line2D
  ( Line2D (Line2D)
  , startPoint
  , endPoint
  , endpoints
  , length
  , length#
  , direction
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
  , offsetLeftwardBy
  , offsetRightwardBy
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Axis2D (Axis2D)
import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Nondegenerate (IsDegenerate (IsDegenerate))
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Transform2D (Transform2D)
import OpenSolid.Transform2D qualified as Transform2D
import OpenSolid.Unboxed.Math
import OpenSolid.UvSpace (UvSpace)
import OpenSolid.Vector2D (Vector2D)

-- | A line in 2D, with a start point and end point.
data Line2D units space
  = -- | Construct a line from its start and end points.
    Line2D (Point2D units space) (Point2D units space)

instance FFI (Line2D Meters FFI.Space) where
  representation = FFI.classRepresentation "Line2D"

instance FFI (Line2D Unitless UvSpace) where
  representation = FFI.classRepresentation "UvLine"

-- | Get the start point of a line.
{-# INLINE startPoint #-}
startPoint :: Line2D units space -> Point2D units space
startPoint (Line2D p1 _) = p1

-- | Get the end point of a line.
{-# INLINE endPoint #-}
endPoint :: Line2D units space -> Point2D units space
endPoint (Line2D _ p2) = p2

-- | Get the start and end points of a line as a tuple.
endpoints :: Line2D units space -> (Point2D units space, Point2D units space)
endpoints (Line2D p1 p2) = (p1, p2)

-- | Get the length of a line.
length :: Line2D units space -> Quantity units
length line = Quantity# (length# line)

{-# INLINE length# #-}
length# :: Line2D units space -> Double#
length# (Line2D p1 p2) = Point2D.distanceFrom# p1 p2

direction :: Tolerance units => Line2D units space -> Result IsDegenerate (Direction2D space)
direction (Line2D p1 p2) =
  case Direction2D.from p1 p2 of
    Ok lineDirection -> Ok lineDirection
    Error Direction2D.PointsAreCoincident -> Error IsDegenerate

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
  case dotProduct# <=# 0.0## of
    1# -> hypot2# ux# uy#
    _ -> case dotProduct# >=# lengthSquared# of
      1# -> hypot2# (x0# -# x2#) (y0# -# y2#)
      _ -> abs# (vx# *# uy# -# vy# *# ux#) /# sqrt# lengthSquared#

reverse :: Line2D units space -> Line2D units space
reverse (Line2D p1 p2) = Line2D p2 p1

transformBy :: Transform2D tag units space -> Line2D units space -> Line2D units space
transformBy transform (Line2D p1 p2) =
  Line2D (Point2D.transformBy transform p1) (Point2D.transformBy transform p2)

translateBy :: Vector2D units space -> Line2D units space -> Line2D units space
translateBy = Transform2D.translateByImpl transformBy

translateIn :: Direction2D space -> Quantity units -> Line2D units space -> Line2D units space
translateIn = Transform2D.translateInImpl transformBy

translateAlong :: Axis2D units space -> Quantity units -> Line2D units space -> Line2D units space
translateAlong = Transform2D.translateAlongImpl transformBy

rotateAround :: Point2D units space -> Angle -> Line2D units space -> Line2D units space
rotateAround = Transform2D.rotateAroundImpl transformBy

mirrorAcross :: Axis2D units space -> Line2D units space -> Line2D units space
mirrorAcross = Transform2D.mirrorAcrossImpl transformBy

scaleAbout :: Point2D units space -> Number -> Line2D units space -> Line2D units space
scaleAbout = Transform2D.scaleAboutImpl transformBy

scaleAlong :: Axis2D units space -> Number -> Line2D units space -> Line2D units space
scaleAlong = Transform2D.scaleAlongImpl transformBy

offsetLeftwardBy ::
  Tolerance units =>
  Quantity units ->
  Line2D units space ->
  Result IsDegenerate (Line2D units space)
offsetLeftwardBy distance line = do
  lineDirection <- direction line
  Ok (translateIn (Direction2D.rotateLeft lineDirection) distance line)

offsetRightwardBy ::
  Tolerance units =>
  Quantity units ->
  Line2D units space ->
  Result IsDegenerate (Line2D units space)
offsetRightwardBy distance = offsetLeftwardBy -distance
