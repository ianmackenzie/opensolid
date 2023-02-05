module Line2d
  ( Line2d (..)
  , startPoint
  , endPoint
  , pointOn
  , segmentBounds
  , reverse
  , bisect
  , boundingBox
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
import Vector2d (Vector2d)
import Vector2d qualified
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

data Line2d units coordinates
  = Line2d (Point2d units coordinates) (Point2d units coordinates)
  deriving (Eq, Show)

startPoint :: Line2d units coordinates -> Point2d units coordinates
startPoint (Line2d p1 _) = p1

endPoint :: Line2d units coordinates -> Point2d units coordinates
endPoint (Line2d _ p2) = p2

pointOn :: Line2d units coordinates -> Float -> Point2d units coordinates
pointOn (Line2d p1 p2) = Point2d.interpolateFrom p1 p2

segmentBounds :: Line2d units coordinates -> Range Unitless -> BoundingBox2d units coordinates
segmentBounds line (Range t1 t2) =
  BoundingBox2d.hull2 (Line2d.pointOn line t1) (Line2d.pointOn line t2)

derivative :: Line2d units coordinates -> VectorCurve2d units coordinates
derivative line = VectorCurve2d.constant (vector line)

reverse :: Line2d units coordinates -> Line2d units coordinates
reverse (Line2d p1 p2) = Line2d p2 p1

bisect :: Line2d units coordinates -> (Line2d units coordinates, Line2d units coordinates)
bisect (Line2d p1 p2) =
  let midpoint = Point2d.midpoint p1 p2
   in (Line2d p1 midpoint, Line2d midpoint p2)

boundingBox :: Line2d units coordinates -> BoundingBox2d units coordinates
boundingBox (Line2d p1 p2) = BoundingBox2d.hull2 p1 p2

instance IsCurve2d (Line2d units coordinates) units coordinates where
  startPointImpl = startPoint
  endPointImpl = endPoint
  pointOnImpl = pointOn
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  bisectImpl = bisect
  boundingBoxImpl = boundingBox

length :: Line2d units coordinates -> Qty units
length (Line2d p1 p2) = Point2d.distanceFrom p1 p2

vector :: Line2d units coordinates -> Vector2d units coordinates
vector (Line2d p1 p2) = p2 - p1

data IsDegenerate = IsDegenerate

direction :: Line2d units coordinates -> Result IsDegenerate (Direction2d coordinates)
direction line = Vector2d.direction (vector line) ?! IsDegenerate

lengthAndDirection :: Line2d units coordinates -> Result IsDegenerate (Qty units, Direction2d coordinates)
lengthAndDirection line = Vector2d.magnitudeAndDirection (vector line) ?! IsDegenerate

axis :: Line2d units coordinates -> Result IsDegenerate (Axis2d units coordinates)
axis line = do
  axisDirection <- direction line
  let axisOrigin = Line2d.startPoint line
  Ok (Axis2d.through axisOrigin axisDirection)
