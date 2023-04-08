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
  , centerPoint
  , radius
  , startAngle
  , endAngle
  , sweptAngle
  , Constraint (..)
  , BuildError
  , Direction (Clockwise, Counterclockwise)
  , Size (Large, Small)
  , fromCenterPointRadiusStartAngleEndAngle
  , fromCenterPointStartPointSweptAngle
  , fromStartPointEndPointRadiusDirectionSize
  , fromStartPointEndPointSweptAngle
  )
where

import Angle (Angle)
import Angle qualified
import BoundingBox2d (BoundingBox2d)
import CoordinateSystem (Units)
import Curve1d qualified
import Curve2d (IsCurve2d (..))
import Direction2d qualified
import Line2d qualified
import List qualified
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

instance IsCurve2d (Arc2d (space @ units)) (space @ units) where
  startPointImpl = startPoint
  endPointImpl = endPoint
  pointOnImpl = pointOn
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  bisectImpl = bisect
  boundingBoxImpl = boundingBox

data Constraint coordinateSystem
  = CenterPoint (Point2d coordinateSystem)
  | StartPoint (Point2d coordinateSystem)
  | EndPoint (Point2d coordinateSystem)
  | Radius (Qty (Units coordinateSystem))
  | StartAngle Angle
  | EndAngle Angle
  | SweptAngle Angle
  | Direction Direction
  | Size Size
  deriving (Eq, Ord)

data Direction = Clockwise | Counterclockwise deriving (Eq, Ord)

data Size = Large | Small deriving (Eq, Ord)

data BuildError
  = UnsupportedConstraints
  | EndpointsCoincident
  | EndpointsTooFarApart
  | ZeroSweptAngle

instance IsError BuildError where
  errorMessage UnsupportedConstraints = "Unsupported set of constraints for Arc2d construction"
  errorMessage EndpointsCoincident = "Given Arc2d endpoints are coincident"
  errorMessage EndpointsTooFarApart = "Given Arc2d endpoints are too far apart"
  errorMessage ZeroSweptAngle = "Given Arc2d swept angle is zero (therefore radius is infinity)"

with :: List (Constraint (space @ units)) -> Result BuildError (Arc2d (space @ units))
with constraints = case List.sort constraints of
  [CenterPoint p0, Radius r, StartAngle theta1, EndAngle theta2] ->
    Ok $ fromCenterPointRadiusStartAngleEndAngle p0 r theta1 theta2
  [CenterPoint p0, Radius r, StartAngle theta1, SweptAngle theta] ->
    Ok $ fromCenterPointRadiusStartAngleEndAngle p0 r theta1 (theta1 + theta)
  [CenterPoint p0, StartPoint p1, SweptAngle theta] ->
    Ok $ fromCenterPointStartPointSweptAngle p0 p1 theta
  [StartPoint p1, EndPoint p2, Radius r, Direction d, Size s] ->
    fromStartPointEndPointRadiusDirectionSize p1 p2 r d s
  [StartPoint p1, EndPoint p2, SweptAngle theta] ->
    fromStartPointEndPointSweptAngle p1 p2 theta
  _ -> Error UnsupportedConstraints

fromCenterPointRadiusStartAngleEndAngle :: Point2d (space @ units) -> Qty units -> Angle -> Angle -> Arc2d (space @ units)
fromCenterPointRadiusStartAngleEndAngle = Arc2d

fromCenterPointStartPointSweptAngle :: Point2d (space @ units) -> Point2d (space @ units) -> Angle -> Arc2d (space @ units)
fromCenterPointStartPointSweptAngle p0 p1 theta =
  let computedStartAngle = Point2d.angleFrom p0 p1
   in Arc2d
        { centerPoint = p0
        , radius = Point2d.distanceFrom p0 p1
        , startAngle = computedStartAngle
        , endAngle = computedStartAngle + theta
        }

fromStartPointEndPointRadiusDirectionSize
  :: Point2d (space @ units)
  -> Point2d (space @ units)
  -> Qty units
  -> Direction
  -> Size
  -> Result BuildError (Arc2d (space @ units))
fromStartPointEndPointRadiusDirectionSize givenStartPoint givenEndPoint givenRadius direction size = do
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
  Ok (fromCenterPointStartPointSweptAngle computedCenterPoint givenStartPoint computedSweptAngle)

fromStartPointEndPointSweptAngle :: Point2d (space @ units) -> Point2d (space @ units) -> Angle -> Result BuildError (Arc2d (space @ units))
fromStartPointEndPointSweptAngle p1 p2 theta = do
  (distance, direction) <- Vector2d.magnitudeAndDirection (p2 - p1) ?? Error EndpointsCoincident
  let tanHalfAngle = Angle.tan (0.5 * theta)
  denominator <- validate (/= Qty.zero) tanHalfAngle ?? Error ZeroSweptAngle
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
