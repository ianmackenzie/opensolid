module OpenSolid.Line
  ( Line (Line)
  , startPoint
  , endPoint
  , endpoints
  , length
  , direction
  , bounds
  , distanceTo
  , reverse
  )
where

import Data.Void (Void)
import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Direction (Direction)
import OpenSolid.Error (IsDegenerate (IsDegenerate))
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Point (Point)
import OpenSolid.Point qualified as Point
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Units qualified as Units
import OpenSolid.Vector qualified as Vector

data Line dimension units space
  = Line (Point dimension units space) (Point dimension units space)

instance FFI (Line 2 Meters Void) where
  representation = FFI.classRepresentation "Line2D"

instance FFI (Line 2 Unitless Void) where
  representation = FFI.classRepresentation "UvLine"

-- | Get the start point of a line.
{-# INLINE startPoint #-}
startPoint :: Line dimension units space -> Point dimension units space
startPoint (Line p1 _) = p1

-- | Get the end point of a line.
{-# INLINE endPoint #-}
endPoint :: Line dimension units space -> Point dimension units space
endPoint (Line _ p2) = p2

-- | Get the start and end points of a line as a tuple.
endpoints :: Line dimension units space -> (Point dimension units space, Point dimension units space)
endpoints (Line p1 p2) = (p1, p2)

-- | Get the length of a line.
length :: Point.Exists dimension units space => Line dimension units space -> Quantity units
length (Line p1 p2) = Point.distanceFrom p1 p2

direction ::
  ( Point.Exists dimension units space
  , Tolerance units
  ) =>
  Line dimension units space ->
  Result IsDegenerate (Direction dimension space)
direction (Line p1 p2) =
  case Vector.direction (p2 - p1) of
    Ok lineDirection -> Ok lineDirection
    Error Vector.IsZero -> Error IsDegenerate

bounds ::
  Point.Exists dimension units space =>
  Line dimension units space ->
  Bounds dimension units space
bounds (Line p1 p2) = Bounds.hull2 p1 p2

{-| Get the distance from a line to a point.

This is measured from the point on the line closest to the given point
(note that the closest point might be within the line,
or might be one of the line's endpoints,
so this is not necessarily a *perpendicular* distance).
-}
distanceTo ::
  Point.Exists dimension units space =>
  Point dimension units space ->
  Line dimension units space ->
  Quantity units
distanceTo p (Line p1 p2) = do
  let d = p - p1
  let v = p2 - p1
  let lengthSquared_ = Vector.squaredMagnitude_ v
  let dotProduct_ = d `dot_` v
  if
    | dotProduct_ <= Quantity.zero -> Point.distanceFrom p1 p
    | dotProduct_ >= lengthSquared_ -> Point.distanceFrom p2 p
    | otherwise -> Units.simplify do
        Vector.crossProductMagnitude_ v d ?/? Quantity.sqrt_ lengthSquared_

reverse :: Line dimension units space -> Line dimension units space
reverse (Line p1 p2) = Line p2 p1
