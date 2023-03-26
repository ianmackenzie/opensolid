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
  , Arguments
  , CenterPoint (CenterPoint)
  , StartPoint (StartPoint)
  , EndPoint (EndPoint)
  , Radius (Radius)
  , StartAngle (StartAngle)
  , EndAngle (EndAngle)
  , SweptAngle (SweptAngle)
  , Direction (Clockwise, Counterclockwise)
  , Size (Large, Small)
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
  , endAngle :: Angle
  }
  deriving (Eq, Show)

sweptAngle :: Arc2d (space @ units) -> Angle
sweptAngle arc = arc.endAngle - arc.startAngle

instance HasField "sweptAngle" (Arc2d (space @ units)) Angle where
  getField = sweptAngle

pointOn :: Arc2d (space @ units) -> Float -> Point2d (space @ units)
pointOn arc t =
  let theta = Qty.interpolateFrom arc.startAngle arc.endAngle t
   in arc.centerPoint + Vector2d.polar arc.radius theta

startPoint :: Arc2d (space @ units) -> Point2d (space @ units)
startPoint arc = pointOn arc 0.0

endPoint :: Arc2d (space @ units) -> Point2d (space @ units)
endPoint arc = pointOn arc 1.0

segmentBounds :: Arc2d (space @ units) -> Range Unitless -> BoundingBox2d (space @ units)
segmentBounds arc t =
  let r = Range.constant arc.radius
      theta = arc.startAngle + t * arc.sweptAngle
   in arc.centerPoint + VectorBox2d.polar r theta

derivative :: Arc2d (space @ units) -> VectorCurve2d (space @ units)
derivative arc =
  let theta = arc.startAngle + Curve1d.parameter * arc.sweptAngle
      r = arc.radius
      x = r * Curve1d.cos theta
      y = r * Curve1d.sin theta
   in VectorCurve2d.xy (Curve1d.derivative x) (Curve1d.derivative y)

reverse :: Arc2d (space @ units) -> Arc2d (space @ units)
reverse arc =
  arc{startAngle = arc.endAngle, endAngle = arc.startAngle}

bisect :: Arc2d (space @ units) -> (Arc2d (space @ units), Arc2d (space @ units))
bisect arc =
  let midAngle = Qty.midpoint arc.startAngle arc.endAngle
   in (arc{endAngle = midAngle}, arc{startAngle = midAngle})

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

newtype CenterPoint (coordinateSystem :: CoordinateSystem) = CenterPoint (Point2d coordinateSystem)

newtype StartPoint (coordinateSystem :: CoordinateSystem) = StartPoint (Point2d coordinateSystem)

newtype EndPoint (coordinateSystem :: CoordinateSystem) = EndPoint (Point2d coordinateSystem)

newtype Radius units = Radius (Qty units)

newtype StartAngle = StartAngle Angle

newtype EndAngle = EndAngle Angle

newtype SweptAngle = SweptAngle Angle

data Direction = Clockwise | Counterclockwise

data Size = Large | Small

instance
  units ~ units'
  => Arguments
      ( CenterPoint (space @ units)
      , Radius units'
      , StartAngle
      , EndAngle
      )
      (Arc2d (space @ units))
  where
  build (CenterPoint givenCenterPoint, Radius givenRadius, StartAngle givenStartAngle, EndAngle givenEndAngle) =
    Arc2d
      { centerPoint = givenCenterPoint
      , radius = givenRadius
      , startAngle = givenStartAngle
      , endAngle = givenEndAngle
      }

instance
  ( space ~ space'
  , units ~ units'
  )
  => Arguments
      ( CenterPoint (space @ units)
      , StartPoint (space' @ units')
      , SweptAngle
      )
      (Arc2d (space @ units))
  where
  build (CenterPoint givenCenterPoint, StartPoint givenStartPoint, SweptAngle givenSweptAngle) =
    let computedStartAngle = Point2d.angleFrom givenCenterPoint givenStartPoint
     in Arc2d
          { centerPoint = givenCenterPoint
          , radius = Point2d.distanceFrom givenCenterPoint givenStartPoint
          , startAngle = computedStartAngle
          , endAngle = computedStartAngle + givenSweptAngle
          }

data EndpointsCoincidentOrTooFarApart = EndpointsCoincident | EndpointsTooFarApart

instance IsError EndpointsCoincidentOrTooFarApart where
  errorMessage EndpointsCoincident = "Given Arc2d endpoints are coincident"
  errorMessage EndpointsTooFarApart = "Given Arc2d endpoints are too far apart"

instance
  ( space ~ space'
  , units ~ units'
  , units ~ units''
  )
  => Arguments
      ( StartPoint (space @ units)
      , EndPoint (space' @ units')
      , Radius units''
      , Direction
      , Size
      )
      (Result EndpointsCoincidentOrTooFarApart (Arc2d (space @ units)))
  where
  build (StartPoint givenStartPoint, EndPoint givenEndPoint, Radius givenRadius, direction, size) = do
    let chord = Line2d.from givenStartPoint givenEndPoint
    chordDirection <- Line2d.direction chord ?? Error EndpointsCoincident
    let squaredRadius = Qty.squared (Units.generalize givenRadius)
    let squaredHalfLength = Qty.squared (Units.generalize (0.5 * Line2d.length chord))
    squaredOffsetMagnitude <- validate (>= Qty.zero) (squaredRadius - squaredHalfLength) ?? Error EndpointsTooFarApart
    let offsetMagnitude = Units.specialize (Qty.sqrt squaredOffsetMagnitude)
    let offsetDirection = Direction2d.perpendicularTo chordDirection
    let offsetDistance =
          case (direction, size) of
            (Counterclockwise, Small) -> offsetMagnitude
            (Clockwise, Small) -> -offsetMagnitude
            (Clockwise, Large) -> offsetMagnitude
            (Counterclockwise, Large) -> -offsetMagnitude
    let computedCenterPoint = Line2d.midpoint chord + offsetDirection * offsetDistance
    let halfLength = Units.specialize (Qty.sqrt squaredHalfLength)
    let shortAngle = 2.0 * Angle.asin (halfLength / givenRadius)
    let computedSweptAngle =
          case (direction, size) of
            (Counterclockwise, Small) -> shortAngle
            (Clockwise, Small) -> -shortAngle
            (Clockwise, Large) -> shortAngle - Angle.fullTurn
            (Counterclockwise, Large) -> Angle.fullTurn - shortAngle
    Ok $
      with
        ( CenterPoint computedCenterPoint
        , StartPoint givenStartPoint
        , SweptAngle computedSweptAngle
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
  let computedCenterPoint = Point2d.midpoint p1 p2 + offset * Direction2d.perpendicularTo direction
  let computedStartAngle = Point2d.angleFrom computedCenterPoint p1
  Ok
    Arc2d
      { centerPoint = computedCenterPoint
      , radius = Point2d.distanceFrom computedCenterPoint p1
      , startAngle = computedStartAngle
      , endAngle = computedStartAngle + theta
      }
