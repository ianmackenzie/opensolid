module OpenSolid.Line2D
  ( Line2D
  , pattern Line2D
  , startPoint
  , endPoint
  , endpoints
  , length
  , direction
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
  , offsetLeftwardBy
  , offsetRightwardBy
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Axis2D (Axis2D)
import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.Line (Line (Line))
import OpenSolid.Line qualified as Line
import OpenSolid.Nondegenerate (IsDegenerate)
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Transform2D (Transform2D)
import OpenSolid.Transform2D qualified as Transform2D
import OpenSolid.Vector2D (Vector2D)

-- | A line in 2D, with a start point and end point.
type Line2D units space =
  Line 2 units space

-- | Construct a line from its start and end points.
{-# INLINE Line2D #-}
pattern Line2D :: Point2D units space -> Point2D units space -> Line2D units space
pattern Line2D p1 p2 = Line p1 p2

{-# COMPLETE Line2D #-}

-- | Get the start point of a line.
{-# INLINE startPoint #-}
startPoint :: Line2D units space -> Point2D units space
startPoint = Line.startPoint

-- | Get the end point of a line.
{-# INLINE endPoint #-}
endPoint :: Line2D units space -> Point2D units space
endPoint = Line.endPoint

-- | Get the start and end points of a line as a tuple.
endpoints :: Line2D units space -> (Point2D units space, Point2D units space)
endpoints = Line.endpoints

-- | Get the length of a line.
length :: Line2D units space -> Quantity units
length = Line.length

direction :: Tolerance units => Line2D units space -> Result IsDegenerate (Direction2D space)
direction = Line.direction

bounds :: Line2D units space -> Bounds2D units space
bounds = Line.bounds

{-| Get the distance from a line to a point.

This is measured from the point on the line closest to the given point
(note that the closest point might be within the line,
or might be one of the line's endpoints,
so this is not necessarily a *perpendicular* distance).
-}
distanceTo :: Point2D units space -> Line2D units space -> Quantity units
distanceTo = Line.distanceTo

reverse :: Line2D units space -> Line2D units space
reverse = Line.reverse

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
