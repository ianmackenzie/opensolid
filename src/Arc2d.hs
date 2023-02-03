module Arc2d
  ( Arc2d
  , startPoint
  , endPoint
  , pointOn
  , segmentBounds
  , reverse
  , bisect
  , boundingBox
  , from
  , centerPoint
  , radius
  , startAngle
  , sweptAngle
  )
where

import Angle qualified
import BoundingBox2d (BoundingBox2d)
import Curve1d qualified
import Curve2d (IsCurve2d (..))
import Direction2d qualified
import Line2d (Line2d (..))
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import Range (Range)
import Range qualified
import Vector2d qualified
import VectorBox2d qualified
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

data Arc2d coordinates = Arc2d
  { centerPoint :: Point2d coordinates
  , radius :: Length
  , startAngle :: Angle
  , sweptAngle :: Angle
  }
  deriving (Eq, Show)

pointOn :: Arc2d coordinates -> Float -> Point2d coordinates
pointOn arc t =
  let r = radius arc
      theta = startAngle arc + t * sweptAngle arc
   in centerPoint arc + Vector2d.polar r theta

startPoint :: Arc2d coordinates -> Point2d coordinates
startPoint arc = pointOn arc 0.0

endPoint :: Arc2d coordinates -> Point2d coordinates
endPoint arc = pointOn arc 1.0

segmentBounds :: Arc2d coordinates -> Range Unitless -> BoundingBox2d coordinates
segmentBounds arc t =
  let r = Range.constant (radius arc)
      theta = startAngle arc + t * sweptAngle arc
   in centerPoint arc + VectorBox2d.polar r theta

derivative :: Arc2d coordinates -> VectorCurve2d Meters coordinates
derivative arc =
  let theta = startAngle arc + Curve1d.parameter * sweptAngle arc
      r = radius arc
      vx = -r * Curve1d.sin theta
      vy = r * Curve1d.cos theta
   in VectorCurve2d.xy vx vy

reverse :: Arc2d coordinates -> Arc2d coordinates
reverse arc =
  arc{startAngle = startAngle arc + sweptAngle arc, sweptAngle = -(sweptAngle arc)}

bisect :: Arc2d coordinates -> (Arc2d coordinates, Arc2d coordinates)
bisect arc =
  let halfSweptAngle = 0.5 * sweptAngle arc
      first = arc{sweptAngle = halfSweptAngle}
      second = arc{sweptAngle = halfSweptAngle, startAngle = startAngle arc + halfSweptAngle}
   in (first, second)

boundingBox :: Arc2d coordinates -> BoundingBox2d coordinates
boundingBox arc = segmentBounds arc (Range.from 0.0 1.0)

instance IsCurve2d (Arc2d coordinates) coordinates where
  startPointImpl = startPoint
  endPointImpl = endPoint
  pointOnImpl = pointOn
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  bisectImpl = bisect
  boundingBoxImpl = boundingBox

from :: Point2d coordinates -> Point2d coordinates -> Angle -> Result (Line2d coordinates) (Arc2d coordinates)
from p1 p2 theta =
  case Vector2d.magnitudeAndDirection (p2 - p1) of
    Err Vector2d.IsZero -> Err (Line2d p1 p2)
    Ok (distance, direction) ->
      let halfAngle = 0.5 * theta
          denominator = Angle.tan halfAngle
       in if denominator == Qty.zero
            then Err (Line2d p1 p2)
            else
              let halfDistance = 0.5 * distance
                  offset = halfDistance / denominator
                  center = Point2d.midpoint p1 p2 + offset * Direction2d.rotateLeft direction
               in Ok
                    Arc2d
                      { centerPoint = center
                      , radius = Point2d.distanceFrom center p1
                      , startAngle = Point2d.angleFrom center p1
                      , sweptAngle = theta
                      }
