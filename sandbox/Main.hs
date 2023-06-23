module Main (main) where

import Angle qualified
import Arc2d qualified
import Area qualified
import Axis2d qualified
import Console qualified
import Curve1d qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Debug qualified
import Direction2d qualified
import Direction3d ()
import Domain (Domain)
import Float qualified
import Length qualified
import Line2d qualified
import List qualified
import OpenSolid
import Parameter1d qualified
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import QuadraticSpline2d qualified
import Random qualified
import Range (Range)
import Range qualified
import Result qualified
import Task qualified
import Text qualified
import Transform2d qualified
import Try qualified
import Units (Meters, Unitless)
import Vector2d qualified
import Vector3d qualified
import VectorCurve2d qualified
import Volume qualified

log :: Show a => Text -> a -> Task Text ()
log label value = Console.print (label ++ ": " ++ Debug.show value)

data WorldSpace

type WorldCoordinates = WorldSpace @ Meters

data MyPoints = MyPoints (Point2d WorldCoordinates) (Point2d WorldCoordinates) deriving (Show)

offsetPoint :: Point2d (space @ units) -> Point2d (space @ units) -> Qty units -> Point2d (space @ units)
offsetPoint startPoint endPoint distance = Result.withDefault startPoint do
  direction <- Direction2d.from startPoint endPoint
  Ok (Point2d.midpoint startPoint endPoint + distance * Direction2d.perpendicularTo direction)

joinTextChunks :: Text -> Text -> Maybe Text
joinTextChunks " " _ = Nothing
joinTextChunks _ " " = Nothing
joinTextChunks s1 s2 = Just (s1 ++ s2)

testListCollapse :: Task Text ()
testListCollapse =
  let textChunks = ["T", "h", "is", " ", "i", "s", " ", "a", " ", "t", "es", "t"]
   in log "Collapsed list" (List.collapse joinTextChunks textChunks |> List.filter (/= " "))

testCurveFind :: Task Text ()
testCurveFind = Try.do
  let p1 = Point2d.meters 0.0 0.0
  let p2 = Point2d.meters 1.0 2.0
  let p3 = Point2d.meters 2.0 0.0
  let testSpline = QuadraticSpline2d.fromControlPoints p1 p2 p3
  [startParameterValue] <- Curve2d.parameterValues Point2d.origin testSpline
  [endParameterValue] <- Curve2d.parameterValues (Point2d.meters 2.0 0.0) testSpline
  [midParameterValue] <- Curve2d.parameterValues (Point2d.meters 1.0 1.0) testSpline
  offCurveParameterValues <- Curve2d.parameterValues (Point2d.meters 1.0 1.1) testSpline
  log "Start parameter value" startParameterValue
  log "End parameter value" endParameterValue
  log "Mid parameter value" midParameterValue
  log "Off-curve parameter values" offCurveParameterValues
 where
  ?tolerance = Length.meters 1e-9

testDirection2dAngleFrom :: Task Text ()
testDirection2dAngleFrom = do
  let angle start end =
        Direction2d.angleFrom
          (Direction2d.degrees (Float.fromInt start))
          (Direction2d.degrees (Float.fromInt end))
          |> Angle.inDegrees
  log "Direction2d.angleFrom (Direction2d.degrees 10) (Direction2d.degrees 30)" (angle 10 30)
  log "Direction2d.angleFrom (Direction2d.degrees 10) (Direction2d.degrees 350)" (angle 10 350)

testArc2dFrom :: Task Text ()
testArc2dFrom = Try.do
  let testArc = Arc2d.from Point2d.origin (Point2d.meters 1.0 1.0)
  let arc1 = testArc (Angle.degrees 90.0)
  let arc2 = testArc (Angle.degrees -90.0)
  let arc3 = testArc (Angle.degrees 180.0)
  let arc4 = testArc (Angle.degrees -180.0)
  let line = testArc Qty.zero
  log "arc1 point" (Curve2d.evaluate arc1 0.5)
  log "arc2 point" (Curve2d.evaluate arc2 0.5)
  log "arc3 point" (Curve2d.evaluate arc3 0.5)
  log "arc4 point" (Curve2d.evaluate arc4 0.5)
  log "line point" (Curve2d.evaluate line 0.5)

overlappingSegments
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> Result Text (List (Domain, Domain))
overlappingSegments curve1 curve2 =
  case Curve2d.intersections curve1 curve2 of
    Ok _ -> Error "Intersection should have failed (and given overlapping segments)"
    Error (Curve2d.OverlappingSegments segments) -> Ok segments
    Error error -> Error (errorMessage error)

testCurveOverlap1 :: Task Text ()
testCurveOverlap1 = Try.do
  arc1 <-
    Arc2d.with
      [ Arc2d.StartPoint (Point2d.meters 1.0 0.0)
      , Arc2d.EndPoint (Point2d.meters -1.0 0.0)
      , Arc2d.SweptAngle (Angle.degrees 180.0)
      ]
  arc2 <-
    Arc2d.with
      [ Arc2d.StartPoint (Point2d.meters 0.0 -1.0)
      , Arc2d.EndPoint (Point2d.meters 0.0 1.0)
      , Arc2d.SweptAngle (Angle.degrees 180.0)
      ]
  segment <- overlappingSegments arc1 arc2
  log "Overlapping segment 1" segment
 where
  ?tolerance = Length.meters 1e-9

testCurveOverlap2 :: Task Text ()
testCurveOverlap2 = Try.do
  arc1 <-
    Arc2d.with
      [ Arc2d.CenterPoint Point2d.origin
      , Arc2d.Radius (Length.meters 1.0)
      , Arc2d.StartAngle (Angle.degrees 0.0)
      , Arc2d.EndAngle (Angle.degrees -180.0)
      ]
  arc2 <-
    Arc2d.with
      [ Arc2d.CenterPoint Point2d.origin
      , Arc2d.Radius (Length.meters 1.0)
      , Arc2d.StartAngle (Angle.degrees -45.0)
      , Arc2d.EndAngle (Angle.degrees 225.0)
      ]
  segment <- overlappingSegments arc1 arc2
  log "Overlapping segment 2" segment
 where
  ?tolerance = Length.meters 1e-9

getCrossProduct :: Result Text Float
getCrossProduct = Try.withContext "In getCrossProduct:" Try.do
  vectorDirection <- Vector2d.direction (Vector2d.meters 2.0 3.0)
  lineDirection <-
    Try.withContext "When getting line direction:" $
      Direction2d.from Point2d.origin Point2d.origin
  Ok (vectorDirection >< lineDirection)

testTry :: Task Text ()
testTry =
  case Try.withContext "In testTry:" getCrossProduct of
    Ok crossProduct -> log "Got cross product" crossProduct
    Error message -> Console.print message

patternMatchError :: Result Text Float
patternMatchError = Try.do
  let line = Line2d.from Point2d.origin (Point2d.meters 3.0 0.0)
  [t1, t2] <- Curve2d.parameterValues (Point2d.meters 1.0 0.0) line
  Ok (t2 - t1)
 where
  ?tolerance = Length.meters 1e-9

testPatternMatchErrorInTryDo :: Task Text ()
testPatternMatchErrorInTryDo =
  log "Pattern match error (expected)" patternMatchError

testCurve1dApproximateEquality :: Task Text ()
testCurve1dApproximateEquality = do
  let t = Curve1d.parameter
  let theta = Angle.radian * t
  log "sin(x) = cos(x)" (Curve1d.sin theta ~= Curve1d.cos theta)
  log "sin(x) = cos(pi/2 - x)" (Curve1d.sin theta ~= Curve1d.cos (Angle.degrees 90.0 - theta))
  let sumOfSquares = Curve1d.squared (Curve1d.sin theta) + Curve1d.squared (Curve1d.cos theta)
  log "sin^2(x) + cos^2(x) = 1" (sumOfSquares ~= 1.0)
  log "sin^2(x) + cos^2(x) = 2" (sumOfSquares ~= 2.0)
 where
  ?tolerance = 1e-9

testTaskIteration :: Task Text ()
testTaskIteration = do
  Task.each (log "Looping") [1 .. 3]

doublingTask :: Text -> Task Text Int
doublingTask input = do
  value <- Text.toInt input
  let doubled = 2 * value
  Task.succeed doubled

doubleManyTask :: Task Text (List Int)
doubleManyTask = do
  Task.collect doublingTask ["1", "-2", "+3"]

testTaskSequencing :: Task Text ()
testTaskSequencing = do
  doubledValues <- doubleManyTask
  Task.each (log "Doubled value") doubledValues

testCurve2dIntersection :: Task Text ()
testCurve2dIntersection = Try.do
  arc1 <-
    Arc2d.with
      [ Arc2d.StartPoint Point2d.origin
      , Arc2d.EndPoint (Point2d.meters 0.0 1.0)
      , Arc2d.SweptAngle (Angle.degrees 180.0)
      ]
  arc2 <-
    Arc2d.with
      [ Arc2d.StartPoint Point2d.origin
      , Arc2d.EndPoint (Point2d.meters 1.0 0.0)
      , Arc2d.SweptAngle (Angle.degrees -180.0)
      ]
  intersections <- Curve2d.intersections arc1 arc2
  Task.each (log "Intersection") intersections
 where
  ?tolerance = Length.meters 1e-9

testCurve2dTangentIntersection :: Task Text ()
testCurve2dTangentIntersection = Try.do
  arc1 <-
    Arc2d.with
      [ Arc2d.CenterPoint Point2d.origin
      , Arc2d.Radius Length.meter
      , Arc2d.StartAngle Qty.zero
      , Arc2d.EndAngle Angle.halfTurn
      ]
  arc2 <-
    Arc2d.with
      [ Arc2d.CenterPoint (Point2d.meters 0.0 1.5)
      , Arc2d.Radius (Length.meters 0.5)
      , Arc2d.StartAngle -Angle.halfTurn
      , Arc2d.EndAngle Qty.zero
      ]
  intersections <- Curve2d.intersections arc1 arc2
  Task.each (log "Tangent intersection") intersections
 where
  ?tolerance = Length.meters 1e-9

testCurve2dSolving :: Task Text ()
testCurve2dSolving = Try.do
  arc <-
    Arc2d.with
      [ Arc2d.StartPoint (Point2d.meters 0.0 1.0)
      , Arc2d.EndPoint (Point2d.meters 1.0 0.0)
      , Arc2d.SweptAngle (Angle.degrees 90.0)
      ]
  let squaredDistanceFromOrigin = VectorCurve2d.squaredMagnitude (arc - Point2d.origin)
  roots <- squaredDistanceFromOrigin |> Curve1d.equalToSquared (Length.meters 0.5)
  log "Curve2d solving roots" roots
 where
  ?tolerance = Length.meters 1e-9

testParameter1d :: Int -> Task Text ()
testParameter1d n = Try.do
  log ("Parameter1d.steps " ++ Text.fromInt n) (Parameter1d.steps n identity)
  log ("Parameter1d.leading " ++ Text.fromInt n) (Parameter1d.leading n identity)
  log ("Parameter1d.trailing " ++ Text.fromInt n) (Parameter1d.trailing n identity)
  log ("Parameter1d.inBetween " ++ Text.fromInt n) (Parameter1d.inBetween n identity)
  log ("Parameter1d.midpoints " ++ Text.fromInt n) (Parameter1d.midpoints n identity)

testParameter1dGeneration :: Task Text ()
testParameter1dGeneration = Try.do
  t1 <- Random.generate Parameter1d.generator
  t2 <- Random.generate Parameter1d.generator
  t3 <- Random.generate Parameter1d.generator
  log "Random parameter value 1" t1
  log "Random parameter value 2" t2
  log "Random parameter value 3" t3

testRangeFind :: Task Text ()
testRangeFind = do
  let isRoot x = Range.includes 2.0 (x * x)
  log "Found square root of 2" (Range.find isRoot (Range.from 1.0 2.0))

script :: Task Text ()
script = do
  log "Integer product" (3 * 4)
  log "Integer division" (10 // 4)
  log "True division" (10 / 4)
  let area = Area.squareMeters 3.0
  let length = Length.centimeters 3.0
  let volume = area * length
  let volumeInCubicCentimeters = Volume.inCubicCentimeters volume
  log "Volume in cubic centimeters" volumeInCubicCentimeters
  let v1 = Vector2d.meters 1.0 2.0
  let v2 = 0.5 * Vector2d.meters 3.0 4.0
  let dotProduct = v1 <> v2
  log "Dot product" dotProduct
  log "2D cross product" (v1 >< v2)
  let squareRoot = Qty.sqrt dotProduct
  log "Square root" squareRoot
  let translatedPoint = Point2d.meters 2.0 3.0 |> Transform2d.translateBy (Vector2d.meters 4.0 5.0)
  log "Translated point" translatedPoint
  let vectorSum = Vector2d.meters 1.0 2.0 + Vector2d.meters 2.0 3.0
  log "Vector sum" vectorSum
  let crossProduct = Vector3d.meters 1.0 2.0 3.0 >< Vector3d.meters 4.0 5.0 6.0
  log "Cross product" crossProduct
  let scaledVector = Length.meters 2.0 * Vector2d.meters 3.0 4.0
  log "Scaled vector" scaledVector
  let rangeDifference = Range.from (Length.meters 2.0) (Length.meters 3.0) - Length.centimeters 50.0
  log "Range difference" rangeDifference
  let rangeProduct = Length.centimeters 20.0 * Range.from (Length.meters 2.0) (Length.meters 3.0)
  log "Range product" rangeProduct
  log "Direction" Direction2d.x
  log "Tuple" (Point2d.meters 1.0 2.0, Point2d.meters 3.0 4.0)
  log "Custom type" (MyPoints (Point2d.meters 1.0 2.0) (Point2d.meters 3.0 4.0))
  let t = Curve1d.parameter
  let x = 3.0 * t
  let y = (x - 1.0) * (x - 1.0) * (x - 1.0) - (x - 1.0)
  let roots = let ?tolerance = 1e-12 in Curve1d.roots y
  log "Roots of (x-1)^3 - (x-1), where x=3*t" roots
  log "sqrt 2.0" (Qty.sqrt 2.0)
  log "Equality test" (let ?tolerance = Length.centimeter in Length.meters 1.0 ~= Length.meters 1.005)
  let theta = Angle.fullTurn * t
  let expression = Curve1d.squared (Curve1d.sin theta)
  let expressionRoots = let ?tolerance = 1e-12 in Curve1d.roots expression
  log "Roots of sin^2(2*pi*t)" expressionRoots
  testCurveFind
  testListCollapse
  Console.print "Unicode output test: ✅❌🙂"
  testDirection2dAngleFrom
  testArc2dFrom
  testCurveOverlap1
  testCurveOverlap2
  log "Rotated axis" (Axis2d.x |> Transform2d.rotateAround (Point2d.meters 1.0 0.0) (Angle.degrees 90.0))
  let originalPoints = [Point2d.meters 1.0 0.0, Point2d.meters 2.0 0.0, Point2d.meters 3.0 0.0]
  let rotationFunction = Transform2d.rotateAround Point2d.origin (Angle.degrees 90.0)
  let rotatedPoints = List.map rotationFunction originalPoints
  log "Rotated points" rotatedPoints
  let transformedAxis =
        Axis2d.x
          |> Transform2d.translateInOwn Axis2d.direction (Length.meters 2.0)
          |> Transform2d.rotateAroundOwn Axis2d.originPoint (Angle.degrees 90.0)
  log "Transformed axis" transformedAxis
  testTry
  testPatternMatchErrorInTryDo
  testCurve1dApproximateEquality
  log "Axis2d.x.originPoint" Axis2d.x.originPoint
  testTaskIteration
  testTaskSequencing
  log "Successive deltas" (List.successive subtract [0, 1, 4, 9, 16, 25])
  log "Successive intervals" (List.successive Range.from [1.0, 2.0, 3.0, 4.0])
  log "Prepend Maybe to List" (Just 1 ++ [2, 3])
  log "Offset point" (offsetPoint (Point2d.meters 1.0 0.0) (Point2d.meters 3.0 0.0) (Length.meters 1.0))
  testCurve2dIntersection
  testCurve2dTangentIntersection
  testCurve2dSolving
  testParameter1d 0
  testParameter1d 1
  testParameter1d 2
  testParameter1d 5
  testParameter1dGeneration
  testRangeFind

main :: IO ()
main = Task.toIO script
