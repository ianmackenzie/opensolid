module Main (main) where

import Angle qualified
import Arc2d qualified
import Area qualified
import Axis2d qualified
import Curve1d qualified
import Curve2d (Curve2d (..))
import Curve2d qualified
import Debug qualified
import Direction2d qualified
import Direction3d ()
import Length qualified
import Line2d qualified
import List qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import QuadraticSpline2d qualified
import Range qualified
import Result qualified
import Script (Script)
import Script qualified
import Text qualified
import Transform2d qualified
import Try qualified
import Units (Meters)
import Vector2d qualified
import Vector3d qualified
import Volume qualified

log :: Show a => Text -> a -> Script ()
log label value = Script.printLine (label ++ ": " ++ Debug.show value)

data World

data MyPoints = MyPoints (Point2d World Meters) (Point2d World Meters) deriving (Show)

listTest :: List (Int, Int)
listTest = List.do
  a <- [1 .. 10]
  b <- [1 .. 10]
  a + b == 10
  [(a, b)]

listFilterTest :: List Int
listFilterTest = List.do
  text <- ["1", "a", "-2", "", "+3"]
  parsed <- Result.toMaybe (Text.toInt text)
  [parsed]

parsingSuccess :: Result Text (List Int)
parsingSuccess = List.do
  text <- ["1", "-2", "+3"]
  parsed <- Text.toInt text
  [parsed]

parsingFailure :: Result Text (List Int)
parsingFailure = List.do
  text <- ["1", "a", "-2", "b", "+3"]
  parsed <- Text.toInt text
  [parsed]

parsingResults :: List (Result Text Int)
parsingResults = List.do
  text <- ["1", "a", "-2", "b", "+3"]
  let parseResult = Text.toInt text
  [parseResult]

joinTextChunks :: Text -> Text -> Maybe Text
joinTextChunks " " _ = Nothing
joinTextChunks _ " " = Nothing
joinTextChunks s1 s2 = Just (s1 ++ s2)

testListCollapse :: Script ()
testListCollapse =
  let textChunks = ["T", "h", "is", " ", "i", "s", " ", "a", " ", "t", "es", "t"]
   in log "Collapsed list" (List.collapse joinTextChunks textChunks |> List.filter (/= " "))

testCurveFind :: Script ()
testCurveFind = Script.do
  let p1 = Point2d.meters 0.0 0.0
  let p2 = Point2d.meters 1.0 2.0
  let p3 = Point2d.meters 2.0 0.0
  let testSpline = Curve2d (QuadraticSpline2d.fromControlPoints p1 p2 p3)
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

testDirection2dAngleFrom :: Script ()
testDirection2dAngleFrom = Script.do
  let angle start end =
        Direction2d.angleFrom (Direction2d.degrees (float start)) (Direction2d.degrees (float end))
          |> Angle.inDegrees
  log "Direction2d.angleFrom (Direction2d.degrees 10) (Direction2d.degrees 30)" (angle 10 30)
  log "Direction2d.angleFrom (Direction2d.degrees 10) (Direction2d.degrees 350)" (angle 10 350)

testArc2dFrom :: Script ()
testArc2dFrom = Script.do
  let arc1 = Arc2d.from Point2d.origin (Point2d.meters 1.0 1.0) (Angle.degrees 90.0)
  log "arc1" arc1
  log "arc2" (Arc2d.from Point2d.origin (Point2d.meters 1.0 1.0) (Angle.degrees -90.0))
  log "arc3" (Arc2d.from Point2d.origin (Point2d.meters 1.0 1.0) (Angle.degrees 180.0))
  log "arc4" (Arc2d.from Point2d.origin (Point2d.meters 1.0 1.0) (Angle.degrees -180.0))
  log "arc1 point" (Result.map (`Arc2d.pointOn` 0.5) arc1)

testCurveOverlap1 :: Script ()
testCurveOverlap1 = Script.do
  arc1 <- Arc2d.from (Point2d.meters 1.0 0.0) (Point2d.meters -1.0 0.0) (Angle.degrees 180.0)
  arc2 <- Arc2d.from (Point2d.meters 0.0 -1.0) (Point2d.meters 0.0 1.0) (Angle.degrees 180.0)
  log "Overlaps" (Curve2d.overlappingSegments (Curve2d arc1) (Curve2d arc2))
 where
  ?tolerance = Length.meters 1e-9

testCurveOverlap2 :: Script ()
testCurveOverlap2 = Script.do
  let arc1 =
        Arc2d.with
          ( #centerPoint Point2d.origin
          , #radius (Length.meters 1.0)
          , #startAngle (Angle.degrees 0.0)
          , #sweptAngle (Angle.degrees -180.0)
          )
  let arc2 =
        Arc2d.with
          ( #centerPoint Point2d.origin
          , #radius (Length.meters 1.0)
          , #startAngle (Angle.degrees -45.0)
          , #sweptAngle (Angle.degrees 270.0)
          )
  log "Overlaps" (Curve2d.overlappingSegments (Curve2d arc1) (Curve2d arc2))
 where
  ?tolerance = Length.meters 1e-9

getCrossProduct :: Result Text Float
getCrossProduct =
  Try.withContext "In getCrossProduct:" $
    Try.do
      vectorDirection <- Vector2d.direction (Vector2d.meters 2.0 3.0)
      lineDirection <- Try.withContext "When getting line direction:" $ Line2d.direction (Line2d.from Point2d.origin Point2d.origin)
      Ok (vectorDirection >< lineDirection)

testTry :: Script ()
testTry =
  case Try.withContext "In testTry:" getCrossProduct of
    Ok crossProduct ->
      log "Got cross product" crossProduct
    Error message ->
      Script.printLine message

patternMatchError :: Result Text Float
patternMatchError = Try.do
  let line = Line2d.from Point2d.origin (Point2d.meters 3.0 0.0)
  [t1, t2] <- Curve2d.parameterValues (Point2d.meters 1.0 0.0) (Curve2d line)
  Ok (t2 - t1)
 where
  ?tolerance = Length.meters 1e-9

testPatternMatchErrorInTryDo :: Script ()
testPatternMatchErrorInTryDo =
  log "Pattern match error" patternMatchError

testCurve1dApproximateEquality :: Script ()
testCurve1dApproximateEquality = Script.do
  let t = Curve1d.parameter
  let theta = Angle.radian * t
  log "sin(x) = cos(x)" (Curve1d.sin theta ~= Curve1d.cos theta)
  log "sin(x) = cos(pi/2 - x)" (Curve1d.sin theta ~= Curve1d.cos (Angle.degrees 90.0 - theta))
  let sumOfSquares = Curve1d.squared (Curve1d.sin theta) + Curve1d.squared (Curve1d.cos theta)
  log "sin^2(x) + cos^2(x) = 1" (sumOfSquares ~= 1.0)
  log "sin^2(x) + cos^2(x) = 2" (sumOfSquares ~= 2.0)
 where
  ?tolerance = 1e-9

script :: Script ()
script = Script.do
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
  let originalPoint = Point2d.origin
  let compositeTransform =
        Transform2d.translationBy (Vector2d.meters 2.0 0.0)
          >> Transform2d.rotationAround Point2d.origin (Angle.degrees 45.0)
          >> Transform2d.scalingAbout Point2d.origin 2.0
  let transformedPoint = Transform2d.scaleBy compositeTransform originalPoint
  log "Transformed point" transformedPoint
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
  log "List test" listTest
  log "Custom type" (MyPoints (Point2d.meters 1.0 2.0) (Point2d.meters 3.0 4.0))
  let t = Curve1d.parameter
  let x = 3.0 * t
  let y = (x - 1.0) * (x - 1.0) * (x - 1.0) - (x - 1.0)
  let roots = let ?tolerance = 1e-12 in Curve1d.roots y
  log "Roots" roots
  log "sqrt 2.0" (Qty.sqrt 2.0)
  log "Equality test" (let ?tolerance = Length.centimeter in Length.meters 1.0 ~= Length.meters 1.005)
  let theta = Angle.fullTurn * t
  let expression = Curve1d.squared (Curve1d.sin theta)
  let expressionRoots = let ?tolerance = 1e-12 in Curve1d.roots expression
  log "Roots" expressionRoots
  testCurveFind
  testListCollapse
  Script.printLine "Unicode output test: ✅❌🙂"
  testDirection2dAngleFrom
  testArc2dFrom
  testCurveOverlap1
  testCurveOverlap2
  log "Rotated axis" (Axis2d.x |> Transform2d.rotateAround (Point2d.meters 1.0 0.0) (Angle.degrees 90.0))
  let originalPoints = [Point2d.meters 1.0 0.0, Point2d.meters 2.0 0.0, Point2d.meters 3.0 0.0]
  let rotationFunction = Transform2d.rotateAround Point2d.origin (Angle.degrees 90.0)
  let rotatedPointsWithFunction = List.map rotationFunction originalPoints
  log "Rotated points with function" rotatedPointsWithFunction
  let rotationTransformation = Transform2d.rotationAround Point2d.origin (Angle.degrees 90.0)
  let rotatedPointsWithTransformation = List.map (Transform2d.transformBy rotationTransformation) originalPoints
  log "Rotated points with transformation" rotatedPointsWithTransformation
  let transformedAxis =
        Axis2d.x
          |> Transform2d.translateInOwn Axis2d.direction (Length.meters 2.0)
          |> Transform2d.rotateAroundOwn Axis2d.originPoint (Angle.degrees 90.0)
  log "Transformed axis" transformedAxis
  testTry
  testPatternMatchErrorInTryDo
  testCurve1dApproximateEquality
  log "Axis2d.x.originPoint" Axis2d.x.originPoint
  log "List filter test" listFilterTest
  log "Parsing success" parsingSuccess
  log "Parsing failure" parsingFailure
  log "Parsing results" parsingResults

main :: IO ()
main = Script.run script
