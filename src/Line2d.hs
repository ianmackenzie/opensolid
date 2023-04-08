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
import Direction2d qualified
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
map function line = Line2d (function line.startPoint) (function line.endPoint)

instance
  (space ~ space', units ~ units')
  => Transformable2d (Line2d (space @ units)) (space' @ units')
  where
  transformBy transformation = map (transformBy transformation)

instance
  (space ~ space', units ~ units')
  => Scalable2d (Line2d (space @ units)) (space' @ units')
  where
  scaleBy scaling = map (scaleBy scaling)

instance
  (space ~ space', units ~ units')
  => Deformable2d (Line2d (space @ units)) (space' @ units')
  where
  deformBy deformation = map (deformBy deformation)

from :: Point2d (space @ units) -> Point2d (space @ units) -> Line2d (space @ units)
from = Line2d

midpoint :: Line2d (space @ units) -> Point2d (space @ units)
midpoint line = Point2d.midpoint line.startPoint line.endPoint

pointOn :: Line2d (space @ units) -> Float -> Point2d (space @ units)
pointOn line t = Point2d.interpolateFrom line.startPoint line.endPoint t

segmentBounds :: Line2d (space @ units) -> Range Unitless -> BoundingBox2d (space @ units)
segmentBounds line range =
  BoundingBox2d.hull2
    (Line2d.pointOn line range.minValue)
    (Line2d.pointOn line range.maxValue)

derivative :: Line2d (space @ units) -> VectorCurve2d (space @ units)
derivative line = VectorCurve2d.constant (vector line)

reverse :: Line2d (space @ units) -> Line2d (space @ units)
reverse line = Line2d line.endPoint line.startPoint

bisect :: Line2d (space @ units) -> (Line2d (space @ units), Line2d (space @ units))
bisect line = let mid = midpoint line in (Line2d line.startPoint mid, Line2d mid line.endPoint)

boundingBox :: Line2d (space @ units) -> BoundingBox2d (space @ units)
boundingBox line = BoundingBox2d.hull2 line.startPoint line.endPoint

instance IsCurve2d (Line2d (space @ units)) (space @ units) where
  startPointImpl = startPoint
  endPointImpl = endPoint
  pointOnImpl = pointOn
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  bisectImpl = bisect
  boundingBoxImpl = boundingBox

length :: Line2d (space @ units) -> Qty units
length line = Point2d.distanceFrom line.startPoint line.endPoint

vector :: Line2d (space @ units) -> Vector2d (space @ units)
vector line = line.endPoint - line.startPoint

data IsDegenerate = IsDegenerate

instance IsError IsDegenerate where
  errorMessage IsDegenerate = "Line2d is degenerate (start and end points are equal)"

direction :: Line2d (space @ units) -> Result IsDegenerate (Direction2d space)
direction line = Direction2d.from line.startPoint line.endPoint ?? Error IsDegenerate

lengthAndDirection :: Line2d (space @ units) -> Result IsDegenerate (Qty units, Direction2d space)
lengthAndDirection line = Vector2d.magnitudeAndDirection (vector line) ?? Error IsDegenerate

axis :: Line2d (space @ units) -> Result IsDegenerate (Axis2d (space @ units))
axis line = do
  axisDirection <- direction line
  Ok (Axis2d.through line.startPoint axisDirection)
