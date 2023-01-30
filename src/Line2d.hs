module Line2d
  ( Line2d (..)
  , Line2d.startPoint
  , Line2d.endPoint
  , Line2d.pointOn
  , Line2d.segmentBounds
  , Line2d.reverse
  , Line2d.bisect
  , Line2d.boundingBox
  , length
  , direction
  , lengthAndDirection
  , axis
  , vector
  )
where

import Axis2d (Axis2d)
import Axis2d qualified
import BoundingBox2d (BoundingBox2d)
import BoundingBox2d qualified
import Curve2d (IsCurve2d (..))
import Direction2d (Direction2d)
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Range (Range (..))
import Result qualified
import Vector2d (Vector2d)
import Vector2d qualified
import VectorCurve2d qualified

data Line2d coordinates
  = Line2d (Point2d coordinates) (Point2d coordinates)

startPoint :: Line2d coordinates -> Point2d coordinates
startPoint (Line2d p1 _) = p1

endPoint :: Line2d coordinates -> Point2d coordinates
endPoint (Line2d _ p2) = p2

pointOn :: Line2d coordinates -> Float -> Point2d coordinates
pointOn (Line2d p1 p2) = Point2d.interpolateFrom p1 p2

segmentBounds :: Line2d coordinates -> Range Unitless -> BoundingBox2d coordinates
segmentBounds line (Range t1 t2) =
  BoundingBox2d.hull2 (Line2d.pointOn line t1) (Line2d.pointOn line t2)

reverse :: Line2d coordinates -> Line2d coordinates
reverse (Line2d p1 p2) = Line2d p2 p1

bisect :: Line2d coordinates -> (Line2d coordinates, Line2d coordinates)
bisect (Line2d p1 p2) =
  let midpoint = Point2d.midpoint p1 p2
   in (Line2d p1 midpoint, Line2d midpoint p2)

boundingBox :: Line2d coordinates -> BoundingBox2d coordinates
boundingBox (Line2d p1 p2) = BoundingBox2d.hull2 p1 p2

instance IsCurve2d Line2d where
  startPoint = Line2d.startPoint
  endPoint = Line2d.endPoint
  pointOn = Line2d.pointOn
  segmentBounds = Line2d.segmentBounds
  derivative line = VectorCurve2d.constant (vector line)
  reverse = Line2d.reverse
  bisect = Line2d.bisect
  boundingBox = Line2d.boundingBox

length :: Line2d coordinates -> Length
length (Line2d p1 p2) = Point2d.distanceFrom p1 p2

vector :: Line2d coordinates -> Vector2d Meters coordinates
vector (Line2d p1 p2) = p2 - p1

data IsDegenerate = IsDegenerate

direction :: Line2d coordinates -> Result IsDegenerate (Direction2d coordinates)
direction line = Vector2d.direction (vector line) |> Result.orErr IsDegenerate

lengthAndDirection :: Line2d coordinates -> Result IsDegenerate (Length, Direction2d coordinates)
lengthAndDirection line = Vector2d.magnitudeAndDirection (vector line) |> Result.orErr IsDegenerate

axis :: Line2d coordinates -> Result IsDegenerate (Axis2d coordinates)
axis line = direction line |> Result.map (Axis2d.through (Line2d.startPoint line))
