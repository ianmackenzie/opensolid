module Line2d
  ( Line2d (startPoint, endPoint)
  , from
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
import Transform2d (Deformable2d (..), Scalable2d (..), Transformable2d (..))
import Units (Unitless)
import Vector2d (Vector2d)
import Vector2d qualified
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

data Line2d (coordinateSystem :: CoordinateSystem) = Line2d
  { startPoint :: Point2d coordinateSystem
  , endPoint :: Point2d coordinateSystem
  }
  deriving (Eq, Show)

map :: (Point2d coordinateSystem1 -> Point2d coordinateSystem2) -> Line2d coordinateSystem1 -> Line2d coordinateSystem2
map function (Line2d p1 p2) = Line2d (function p1) (function p2)

instance
  (space ~ space', units ~ units')
  => Transformable2d (Line2d (Coordinates space units)) space' units'
  where
  transformBy transformation = map (transformBy transformation)

instance
  (space ~ space', units ~ units')
  => Scalable2d (Line2d (Coordinates space units)) space' units'
  where
  scaleBy scaling = map (scaleBy scaling)

instance
  (space ~ space', units ~ units')
  => Deformable2d (Line2d (Coordinates space units)) space' units'
  where
  deformBy deformation = map (deformBy deformation)

from :: Point2d (Coordinates space units) -> Point2d (Coordinates space units) -> Line2d (Coordinates space units)
from = Line2d

midpoint :: Line2d (Coordinates space units) -> Point2d (Coordinates space units)
midpoint (Line2d p1 p2) = Point2d.midpoint p1 p2

pointOn :: Line2d (Coordinates space units) -> Float -> Point2d (Coordinates space units)
pointOn (Line2d p1 p2) = Point2d.interpolateFrom p1 p2

segmentBounds :: Line2d (Coordinates space units) -> Range Unitless -> BoundingBox2d (Coordinates space units)
segmentBounds line (Range t1 t2) =
  BoundingBox2d.hull2 (Line2d.pointOn line t1) (Line2d.pointOn line t2)

derivative :: Line2d (Coordinates space units) -> VectorCurve2d (Coordinates space units)
derivative line = VectorCurve2d.constant (vector line)

reverse :: Line2d (Coordinates space units) -> Line2d (Coordinates space units)
reverse (Line2d p1 p2) = Line2d p2 p1

bisect :: Line2d (Coordinates space units) -> (Line2d (Coordinates space units), Line2d (Coordinates space units))
bisect (Line2d p1 p2) =
  let mid = Point2d.midpoint p1 p2
   in (Line2d p1 mid, Line2d mid p2)

boundingBox :: Line2d (Coordinates space units) -> BoundingBox2d (Coordinates space units)
boundingBox (Line2d p1 p2) = BoundingBox2d.hull2 p1 p2

instance IsCurve2d (Line2d (Coordinates space units)) space units where
  startPointImpl = startPoint
  endPointImpl = endPoint
  pointOnImpl = pointOn
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  bisectImpl = bisect
  boundingBoxImpl = boundingBox

length :: Line2d (Coordinates space units) -> Qty units
length (Line2d p1 p2) = Point2d.distanceFrom p1 p2

vector :: Line2d (Coordinates space units) -> Vector2d (Coordinates space units)
vector (Line2d p1 p2) = p2 - p1

data IsDegenerate = IsDegenerate

instance IsError IsDegenerate where
  errorMessage IsDegenerate = "Line2d is degenerate (start and end points are equal)"

direction :: Line2d (Coordinates space units) -> Result IsDegenerate (Direction2d space)
direction line = Vector2d.direction (vector line) ?? Error IsDegenerate

lengthAndDirection :: Line2d (Coordinates space units) -> Result IsDegenerate (Qty units, Direction2d space)
lengthAndDirection line = Vector2d.magnitudeAndDirection (vector line) ?? Error IsDegenerate

axis :: Line2d (Coordinates space units) -> Result IsDegenerate (Axis2d (Coordinates space units))
axis line = do
  axisDirection <- direction line
  Ok (Axis2d.through line.startPoint axisDirection)
