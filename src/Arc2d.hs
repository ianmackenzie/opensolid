module Arc2d
  ( Arc2d
  , startPoint
  , endPoint
  , pointOn
  , segmentBounds
  , reverse
  , bisect
  , boundingBox
  , with
  , from
  , centerPoint
  , radius
  , startAngle
  , sweptAngle
  )
where

import Angle (Angle)
import Angle qualified
import BoundingBox2d (BoundingBox2d)
import Curve1d qualified
import Curve2d (IsCurve2d (..))
import Direction2d qualified
import Line2d (Line2d)
import Line2d qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import Range (Range)
import Range qualified
import Result qualified
import Units (Unitless)
import Units qualified
import Vector2d qualified
import VectorBox2d qualified
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

type role Arc2d nominal nominal

type Arc2d :: Type -> Type -> Type
data Arc2d coordinates units = Arc2d
  { centerPoint :: Point2d coordinates units
  , radius :: Qty units
  , startAngle :: Angle
  , sweptAngle :: Angle
  }
  deriving (Eq, Show)

pointOn :: Arc2d coordinates units -> Float -> Point2d coordinates units
pointOn arc t =
  let theta = startAngle arc + t * sweptAngle arc
   in centerPoint arc + Vector2d.polar (radius arc) theta

startPoint :: Arc2d coordinates units -> Point2d coordinates units
startPoint arc = pointOn arc 0.0

endPoint :: Arc2d coordinates units -> Point2d coordinates units
endPoint arc = pointOn arc 1.0

segmentBounds :: Arc2d coordinates units -> Range Unitless -> BoundingBox2d coordinates units
segmentBounds arc t =
  let r = Range.constant (radius arc)
      theta = startAngle arc + t * sweptAngle arc
   in centerPoint arc + VectorBox2d.polar r theta

derivative :: Arc2d coordinates units -> VectorCurve2d coordinates units
derivative arc =
  let theta = startAngle arc + Curve1d.parameter * sweptAngle arc
      r = radius arc
      x = r * Curve1d.cos theta
      y = r * Curve1d.sin theta
   in VectorCurve2d.xy (Curve1d.derivative x) (Curve1d.derivative y)

reverse :: Arc2d coordinates units -> Arc2d coordinates units
reverse arc =
  arc{startAngle = startAngle arc + sweptAngle arc, sweptAngle = -(sweptAngle arc)}

bisect :: Arc2d coordinates units -> (Arc2d coordinates units, Arc2d coordinates units)
bisect arc =
  let halfSweptAngle = 0.5 * sweptAngle arc
      first = arc{sweptAngle = halfSweptAngle}
      second = arc{sweptAngle = halfSweptAngle, startAngle = startAngle arc + halfSweptAngle}
   in (first, second)

boundingBox :: Arc2d coordinates units -> BoundingBox2d coordinates units
boundingBox arc = segmentBounds arc (Range.from 0.0 1.0)

instance IsCurve2d (Arc2d coordinates units) coordinates units where
  startPointImpl = startPoint
  endPointImpl = endPoint
  pointOnImpl = pointOn
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  bisectImpl = bisect
  boundingBoxImpl = boundingBox

class Arguments a b | a -> b where
  build :: a -> b

with :: Arguments a b => a -> b
with = build

instance
  ( centerPoint ~ Point2d coordinates units
  , radius ~ Qty units
  )
  => Arguments
      ( Named "centerPoint" centerPoint
      , Named "radius" radius
      , Named "startAngle" Angle
      , Named "sweptAngle" Angle
      )
      (Arc2d coordinates units)
  where
  build (Named givenCenterPoint, Named givenRadius, Named givenStartAngle, Named givenSweptAngle) =
    Arc2d
      { centerPoint = givenCenterPoint
      , radius = givenRadius
      , startAngle = givenStartAngle
      , sweptAngle = givenSweptAngle
      }

instance
  ( centerPoint ~ Point2d coordinates units
  , startPoint ~ Point2d coordinates units
  )
  => Arguments
      ( Named "centerPoint" centerPoint
      , Named "startPoint" startPoint
      , Named "sweptAngle" Angle
      )
      (Arc2d coordinates units)
  where
  build (Named givenCenterPoint, Named givenStartPoint, Named givenSweptAngle) =
    Arc2d
      { centerPoint = givenCenterPoint
      , radius = Point2d.distanceFrom givenCenterPoint givenStartPoint
      , startAngle = Point2d.angleFrom givenCenterPoint givenStartPoint
      , sweptAngle = givenSweptAngle
      }

data EndpointsCoincidentOrTooFarApart = EndpointsCoincident | EndpointsTooFarApart

instance
  ( startPoint ~ Point2d coordinates units
  , endPoint ~ Point2d coordinates units
  , radius ~ Qty units
  )
  => Arguments
      ( Named "startPoint" startPoint
      , Named "endPoint" endPoint
      , Named "radius" radius
      , Named "angleSign" Sign
      , Named "largeAngle" Bool
      )
      (Result EndpointsCoincidentOrTooFarApart (Arc2d coordinates units))
  where
  build (Named givenStartPoint, Named givenEndPoint, Named givenRadius, Named angleSign, Named largeAngle) = Result.do
    let chord = Line2d.from givenStartPoint givenEndPoint
    chordDirection <- Line2d.direction chord ?? Error EndpointsCoincident
    let squaredRadius = Qty.squared (Units.generalize givenRadius)
    let squaredHalfLength = Qty.squared (Units.generalize (0.5 * Line2d.length chord))
    squaredOffsetMagnitude <- validate (>= Qty.zero) (squaredRadius - squaredHalfLength) ?? Error EndpointsTooFarApart
    let offsetMagnitude = Units.specialize (Qty.sqrt squaredOffsetMagnitude)
    let offsetDirection = Direction2d.rotateLeft chordDirection
    let offsetDistance =
          case (angleSign, largeAngle) of
            (Positive, False) -> offsetMagnitude
            (Negative, False) -> -offsetMagnitude
            (Negative, True) -> offsetMagnitude
            (Positive, True) -> -offsetMagnitude
    let computedCenterPoint = Line2d.midpoint chord + offsetDirection * offsetDistance
    let halfLength = Units.specialize (Qty.sqrt squaredHalfLength)
    let shortAngle = 2.0 * Angle.asin (halfLength / givenRadius)
    let computedSweptAngle =
          case (angleSign, largeAngle) of
            (Positive, False) -> shortAngle
            (Negative, False) -> -shortAngle
            (Negative, True) -> shortAngle - Angle.fullTurn
            (Positive, True) -> Angle.fullTurn - shortAngle
    Ok $
      with
        ( #centerPoint computedCenterPoint
        , #startPoint givenStartPoint
        , #sweptAngle computedSweptAngle
        )

from :: Point2d coordinates units -> Point2d coordinates units -> Angle -> Result (Line2d coordinates units) (Arc2d coordinates units)
from p1 p2 theta = Result.do
  (distance, direction) <- Vector2d.magnitudeAndDirection (p2 - p1) ?? Error (Line2d.from p1 p2)
  let tanHalfAngle = Angle.tan (0.5 * theta)
  denominator <- validate (/= Qty.zero) tanHalfAngle ?? Error (Line2d.from p1 p2)
  let offset = 0.5 * distance / denominator
  let center = Point2d.midpoint p1 p2 + offset * Direction2d.rotateLeft direction
  Ok
    Arc2d
      { centerPoint = center
      , radius = Point2d.distanceFrom center p1
      , startAngle = Point2d.angleFrom center p1
      , sweptAngle = theta
      }
