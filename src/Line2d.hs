module Line2d
  ( Line2d
  , from
  , startPoint
  , endPoint
  , midpoint
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
  , IsDegenerate
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
import Transform2d (Deformation2d (..), Scaling2d (..), Transformation2d (..))
import Units (Unitless)
import Vector2d (Vector2d)
import Vector2d qualified
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

data Line2d coordinates units
  = Line2d (Point2d coordinates units) (Point2d coordinates units)
  deriving (Eq, Show)

map :: (Point2d coordinates units -> Point2d coordinates' units') -> Line2d coordinates units -> Line2d coordinates' units'
map function (Line2d p1 p2) = Line2d (function p1) (function p2)

instance
  (coordinates ~ coordinates', units ~ units')
  => Transformation2d (Line2d coordinates units) coordinates' units'
  where
  transformBy transformation = map (transformBy transformation)

instance
  (coordinates ~ coordinates', units ~ units')
  => Scaling2d (Line2d coordinates units) coordinates' units'
  where
  scaleBy scaling = map (scaleBy scaling)

instance
  (coordinates ~ coordinates', units ~ units')
  => Deformation2d (Line2d coordinates units) coordinates' units'
  where
  deformBy deformation = map (deformBy deformation)

from :: Point2d coordinates units -> Point2d coordinates units -> Line2d coordinates units
from = Line2d

startPoint :: Line2d coordinates units -> Point2d coordinates units
startPoint (Line2d p1 _) = p1

endPoint :: Line2d coordinates units -> Point2d coordinates units
endPoint (Line2d _ p2) = p2

midpoint :: Line2d coordinates units -> Point2d coordinates units
midpoint (Line2d p1 p2) = Point2d.midpoint p1 p2

pointOn :: Line2d coordinates units -> Float -> Point2d coordinates units
pointOn (Line2d p1 p2) = Point2d.interpolateFrom p1 p2

segmentBounds :: Line2d coordinates units -> Range Unitless -> BoundingBox2d coordinates units
segmentBounds line (Range t1 t2) =
  BoundingBox2d.hull2 (Line2d.pointOn line t1) (Line2d.pointOn line t2)

derivative :: Line2d coordinates units -> VectorCurve2d coordinates units
derivative line = VectorCurve2d.constant (vector line)

reverse :: Line2d coordinates units -> Line2d coordinates units
reverse (Line2d p1 p2) = Line2d p2 p1

bisect :: Line2d coordinates units -> (Line2d coordinates units, Line2d coordinates units)
bisect (Line2d p1 p2) =
  let mid = Point2d.midpoint p1 p2
   in (Line2d p1 mid, Line2d mid p2)

boundingBox :: Line2d coordinates units -> BoundingBox2d coordinates units
boundingBox (Line2d p1 p2) = BoundingBox2d.hull2 p1 p2

instance IsCurve2d (Line2d coordinates units) coordinates units where
  startPointImpl = startPoint
  endPointImpl = endPoint
  pointOnImpl = pointOn
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  bisectImpl = bisect
  boundingBoxImpl = boundingBox

length :: Line2d coordinates units -> Qty units
length (Line2d p1 p2) = Point2d.distanceFrom p1 p2

vector :: Line2d coordinates units -> Vector2d coordinates units
vector (Line2d p1 p2) = p2 - p1

data IsDegenerate = IsDegenerate

instance IsError IsDegenerate where
  errorMessage IsDegenerate = "Line2d is degenerate (start and end points are equal)"

direction :: Line2d coordinates units -> Result IsDegenerate (Direction2d coordinates)
direction line = Vector2d.direction (vector line) ?? Error IsDegenerate

lengthAndDirection :: Line2d coordinates units -> Result IsDegenerate (Qty units, Direction2d coordinates)
lengthAndDirection line = Vector2d.magnitudeAndDirection (vector line) ?? Error IsDegenerate

axis :: Line2d coordinates units -> Result IsDegenerate (Axis2d coordinates units)
axis line = Result.do
  axisDirection <- direction line
  let axisOrigin = Line2d.startPoint line
  Ok (Axis2d.through axisOrigin axisDirection)
