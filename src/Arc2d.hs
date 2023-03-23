module Arc2d
  ( Arc2d
  , EndpointsCoincidentOrTooFarApart (..)
  , IsLine (..)
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
import CoordinateSystem (Units)
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
import Units (Unitless)
import Units qualified
import Vector2d qualified
import VectorBox2d qualified
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

type role Arc2d nominal

data Arc2d (coordinateSystem :: CoordinateSystem) = Arc2d
  { centerPoint :: Point2d coordinateSystem
  , radius :: Qty (Units coordinateSystem)
  , startAngle :: Angle
  , sweptAngle :: Angle
  }
  deriving (Eq, Show)

pointOn :: Arc2d (space @ units) -> Float -> Point2d (space @ units)
pointOn arc t =
  let theta = startAngle arc + t * sweptAngle arc
   in centerPoint arc + Vector2d.polar (radius arc) theta

startPoint :: Arc2d (space @ units) -> Point2d (space @ units)
startPoint arc = pointOn arc 0.0

endPoint :: Arc2d (space @ units) -> Point2d (space @ units)
endPoint arc = pointOn arc 1.0

segmentBounds :: Arc2d (space @ units) -> Range Unitless -> BoundingBox2d (space @ units)
segmentBounds arc t =
  let r = Range.constant (radius arc)
      theta = startAngle arc + t * sweptAngle arc
   in centerPoint arc + VectorBox2d.polar r theta

derivative :: Arc2d (space @ units) -> VectorCurve2d (space @ units)
derivative arc =
  let theta = startAngle arc + Curve1d.parameter * sweptAngle arc
      r = radius arc
      x = r * Curve1d.cos theta
      y = r * Curve1d.sin theta
   in VectorCurve2d.xy (Curve1d.derivative x) (Curve1d.derivative y)

reverse :: Arc2d (space @ units) -> Arc2d (space @ units)
reverse arc =
  arc{startAngle = startAngle arc + sweptAngle arc, sweptAngle = -(sweptAngle arc)}

bisect :: Arc2d (space @ units) -> (Arc2d (space @ units), Arc2d (space @ units))
bisect arc =
  let halfSweptAngle = 0.5 * sweptAngle arc
      first = arc{sweptAngle = halfSweptAngle}
      second = arc{sweptAngle = halfSweptAngle, startAngle = startAngle arc + halfSweptAngle}
   in (first, second)

boundingBox :: Arc2d (space @ units) -> BoundingBox2d (space @ units)
boundingBox arc = segmentBounds arc (Range.from 0.0 1.0)

instance IsCurve2d (Arc2d (space @ units)) space units where
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
  ( centerPoint ~ Point2d (space @ units)
  , radius ~ Qty units
  )
  => Arguments
      ( Named "centerPoint" centerPoint
      , Named "radius" radius
      , Named "startAngle" Angle
      , Named "sweptAngle" Angle
      )
      (Arc2d (space @ units))
  where
  build (Named givenCenterPoint, Named givenRadius, Named givenStartAngle, Named givenSweptAngle) =
    Arc2d
      { centerPoint = givenCenterPoint
      , radius = givenRadius
      , startAngle = givenStartAngle
      , sweptAngle = givenSweptAngle
      }

instance
  ( centerPoint ~ Point2d (space @ units)
  , startPoint ~ Point2d (space @ units)
  )
  => Arguments
      ( Named "centerPoint" centerPoint
      , Named "startPoint" startPoint
      , Named "sweptAngle" Angle
      )
      (Arc2d (space @ units))
  where
  build (Named givenCenterPoint, Named givenStartPoint, Named givenSweptAngle) =
    Arc2d
      { centerPoint = givenCenterPoint
      , radius = Point2d.distanceFrom givenCenterPoint givenStartPoint
      , startAngle = Point2d.angleFrom givenCenterPoint givenStartPoint
      , sweptAngle = givenSweptAngle
      }

data EndpointsCoincidentOrTooFarApart = EndpointsCoincident | EndpointsTooFarApart

instance IsError EndpointsCoincidentOrTooFarApart where
  errorMessage EndpointsCoincident = "Given Arc2d endpoints are coincident"
  errorMessage EndpointsTooFarApart = "Given Arc2d endpoints are too far apart"

instance
  ( startPoint ~ Point2d (space @ units)
  , endPoint ~ Point2d (space @ units)
  , radius ~ Qty units
  )
  => Arguments
      ( Named "startPoint" startPoint
      , Named "endPoint" endPoint
      , Named "radius" radius
      , Named "angleSign" Sign
      , Named "largeAngle" Bool
      )
      (Result EndpointsCoincidentOrTooFarApart (Arc2d (space @ units)))
  where
  build (Named givenStartPoint, Named givenEndPoint, Named givenRadius, Named angleSign, Named largeAngle) = do
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

newtype IsLine (coordinateSystem :: CoordinateSystem) = IsLine (Line2d coordinateSystem) deriving (Eq, Show)

instance IsError (IsLine (space @ units)) where
  errorMessage (IsLine _) = "Arc is actually just a straight line"

from :: Point2d (space @ units) -> Point2d (space @ units) -> Angle -> Result (IsLine (space @ units)) (Arc2d (space @ units))
from p1 p2 theta = do
  (distance, direction) <- Vector2d.magnitudeAndDirection (p2 - p1) ?? Error (IsLine (Line2d.from p1 p2))
  let tanHalfAngle = Angle.tan (0.5 * theta)
  denominator <- validate (/= Qty.zero) tanHalfAngle ?? Error (IsLine (Line2d.from p1 p2))
  let offset = 0.5 * distance / denominator
  let center = Point2d.midpoint p1 p2 + offset * Direction2d.rotateLeft direction
  Ok
    Arc2d
      { centerPoint = center
      , radius = Point2d.distanceFrom center p1
      , startAngle = Point2d.angleFrom center p1
      , sweptAngle = theta
      }
