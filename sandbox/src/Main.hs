module Main (main) where

import Angle qualified
import Arc2d qualified
import Area qualified
import Axis2d qualified
import Bounds2d qualified
import Colour qualified
import Console qualified
import Curve2d qualified
import Direction2d qualified
import Direction3d ()
import Drawing2d qualified
import File qualified
import Length qualified
import Line2d qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Point2d (Point2d (Point2d))
import Point2d qualified
import Qty qualified
import Random qualified
import Range qualified
import Result qualified
import String qualified
import Surface1d.Function qualified
import Surface1d.Solution qualified
import T qualified
import Task qualified
import Transform2d qualified
import Try qualified
import Units (Meters)
import Uv (Parameter (U, V))
import Vector2d qualified
import Vector3d qualified
import Volume qualified

log :: (Show a) => String -> a -> Task String ()
log label value = Console.printLine (label ++ ": " ++ show value)

testScalarArithmetic :: Task String ()
testScalarArithmetic = Try.do
  log "Integer product" (3 * 4)
  log "Integer division" (10 // 4)
  log "True division" (10 / 4)
  let area = Area.squareMeters 3.0
  let length = Length.centimeters 3.0
  let volume = area * length
  let volumeInCubicCentimeters = Volume.inCubicCentimeters volume
  log "Volume in cubic centimeters" volumeInCubicCentimeters
  log "sqrt 2.0" (Qty.sqrt 2.0)

testVectorArithmetic :: Task String ()
testVectorArithmetic = Try.do
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

testRangeArithmetic :: Task String ()
testRangeArithmetic = Try.do
  let rangeDifference = Range.from (Length.meters 2.0) (Length.meters 3.0) - Length.centimeters 50.0
  log "Range difference" rangeDifference
  let rangeProduct = Length.centimeters 20.0 * Range.from (Length.meters 2.0) (Length.meters 3.0)
  log "Range product" rangeProduct

testEquality :: Task String ()
testEquality = Try.do
  log "Equality test" $
    let ?tolerance = Length.centimeter in Length.meters 1.0 ~= Length.meters 1.005

testTransformation :: Task String ()
testTransformation = Try.do
  log "Rotated axis" $
    (Axis2d.x |> Transform2d.rotateAround (Point2d.meters 1.0 0.0) Angle.quarterTurn)
  let originalPoints = [Point2d.meters 1.0 0.0, Point2d.meters 2.0 0.0, Point2d.meters 3.0 0.0]
  let rotationFunction = Transform2d.rotateAround Point2d.origin Angle.quarterTurn
  let rotatedPoints = List.map rotationFunction originalPoints
  log "Rotated points" rotatedPoints
  let transformedAxis =
        Axis2d.x
          |> Transform2d.translateInOwn Axis2d.direction (Length.meters 2.0)
          |> Transform2d.rotateAroundOwn Axis2d.originPoint Angle.quarterTurn
  log "Transformed axis" transformedAxis

offsetPoint ::
  (Tolerance units) =>
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Qty units ->
  Point2d (space @ units)
offsetPoint startPoint endPoint distance = Result.withDefault startPoint do
  direction <- Direction2d.from startPoint endPoint
  Ok (Point2d.midpoint startPoint endPoint + distance * Direction2d.perpendicularTo direction)

testCustomFunction :: (Tolerance Meters) => Task String ()
testCustomFunction = Try.do
  log "Offset point" $
    offsetPoint (Point2d.meters 1.0 0.0) (Point2d.meters 3.0 0.0) (Length.meters 1.0)

testListOperations :: Task String ()
testListOperations = Try.do
  log "Successive deltas" (List.successive subtract [0, 1, 4, 9, 16, 25])
  log "Successive intervals" (List.successive Range.from [1.0, 2.0, 3.0, 4.0])
  log "Prepend Maybe to List" (Just 1 ++ [2, 3])

getCrossProduct :: (Tolerance Meters) => Result String Float
getCrossProduct = Try.withContext "In getCrossProduct" Try.do
  vectorDirection <- Vector2d.direction (Vector2d.meters 2.0 3.0)
  lineDirection <-
    Try.withContext "When getting line direction" $
      Direction2d.from Point2d.origin Point2d.origin
  Ok (vectorDirection >< lineDirection)

testTry :: (Tolerance Meters) => Task String ()
testTry =
  case Try.withContext "In testTry" getCrossProduct of
    Ok crossProduct -> log "Got cross product" crossProduct
    Error message -> Console.printLine message

testTaskIteration :: Task String ()
testTaskIteration = do
  Task.forEach [1 .. 3] (log "Looping")

doublingTask :: String -> Task String Int
doublingTask input = do
  value <- Task.evaluate (String.toInt input)
  let doubled = 2 * value
  return doubled

doubleManyTask :: Task String (List Int)
doubleManyTask = do
  Task.collect doublingTask ["1", "-2", "3"]

testTaskSequencing :: Task String ()
testTaskSequencing = do
  doubledValues <- doubleManyTask
  Task.forEach doubledValues (log "Doubled value")

testParameter1dGeneration :: Task String ()
testParameter1dGeneration = Try.do
  t1 <- Random.generate T.generator
  t2 <- Random.generate T.generator
  t3 <- Random.generate T.generator
  log "Random parameter value 1" t1
  log "Random parameter value 2" t2
  log "Random parameter value 3" t3

testEmptyCheck :: List Int -> Task String ()
testEmptyCheck [] = Console.printLine "List is empty"
testEmptyCheck (NonEmpty nonEmpty) =
  Console.printLine ("List is non-empty, maximum is " ++ show (NonEmpty.maximum nonEmpty))

testNonEmpty :: Task String ()
testNonEmpty = Try.do
  testEmptyCheck []
  testEmptyCheck [2, 3, 1]

outputLine :: Point2d (space @ Unitless) -> String
outputLine (Point2d px py) = String.fromFloat px ++ "," ++ String.fromFloat py

testSurface1dIntersection :: Task String ()
testSurface1dIntersection = Try.do
  let u = Surface1d.Function.parameter U
  let v = Surface1d.Function.parameter V
  let x = -0.97 + 1.94 * u
  let y = -0.97 + 1.94 * v
  let f = Surface1d.Function.squared x + Surface1d.Function.squared y - 1.0
  solutions <- Task.evaluate (Surface1d.Function.solve f)
  log "Number of solutions" (List.length solutions)
  let segmentPoints segment = [Curve2d.pointOn segment uValue | uValue <- T.steps 10]
  let curvePoints segments = List.collect segmentPoints (NonEmpty.toList segments)
  let solutionPoints = \case
        Surface1d.Solution.CrossingCurve {segments} -> Ok (curvePoints segments)
        Surface1d.Solution.CrossingLoop {segments} -> Ok (curvePoints segments)
        solution -> Error ("Unexpected solution: " ++ show solution)
  solutionPointLists <- Task.evaluate (Result.collect solutionPoints solutions)
  let allSolutionPoints = List.concat solutionPointLists
  let outputLines = List.map outputLine allSolutionPoints
  let fileName = "solution-points.txt"
  File.writeTo fileName (String.join "\n" outputLines)
  File.delete fileName -- Uncomment to actually get the data!
 where
  ?tolerance = 1e-9

testSvgOutput :: Task String ()
testSvgOutput = Try.do
  Drawing2d.writeTo "test.svg" (Bounds2d.hull2 Point2d.origin (Point2d.centimeters 30.0 30.0)) $
    [ Drawing2d.group [] []
    , Drawing2d.group [] $
        [ Drawing2d.polygon [Drawing2d.fillColour Colour.blue] $
            [ Point2d.centimeters 10.0 10.0
            , Point2d.centimeters 20.0 10.0
            , Point2d.centimeters 15.0 20.0
            ]
        ]
    ]

testLineFromEndpoints :: (Tolerance Meters) => Task String ()
testLineFromEndpoints = Try.do
  line1 <-
    Task.evaluate $
      Line2d.with
        ( Line2d.startPoint Point2d.origin
        , Line2d.endPoint (Point2d.centimeters 40.0 30.0)
        )
  case line1 of
    Curve2d.Line {length} ->
      log "Line length in centimeters" (Length.inCentimeters length)
    _ -> log "Unexpected curve" line1

testDirectedLine :: (Tolerance Meters) => Task String ()
testDirectedLine = Try.do
  let line1 =
        Line2d.with
          ( Line2d.startPoint Point2d.origin
          , Line2d.direction (Direction2d.degrees 45.0)
          , Line2d.length (Length.centimeters 200.0)
          )
  case line1 of
    Curve2d.Line {endPoint} -> log "Line end point" endPoint
    _ -> log "Unexpected curve" line1

testArcFromEndpoints :: (Tolerance Meters) => Task String ()
testArcFromEndpoints = Try.do
  arc <-
    Task.evaluate $
      Arc2d.with
        ( Arc2d.startPoint Point2d.origin
        , Arc2d.endPoint (Point2d.centimeters 50.0 50.0)
        , Arc2d.sweptAngle Angle.quarterTurn
        )
  case arc of
    Curve2d.Arc {centerPoint} ->
      log "Arc center point" centerPoint
    _ -> log "Unexpected curve" arc

script :: Task String ()
script = Try.do
  testScalarArithmetic
  testVectorArithmetic
  testRangeArithmetic
  testEquality
  testTransformation
  testTry
  testTaskIteration
  testTaskSequencing
  testCustomFunction
  testListOperations
  testParameter1dGeneration
  testNonEmpty
  testSurface1dIntersection
  testSvgOutput
  testLineFromEndpoints
  testDirectedLine
  testArcFromEndpoints
 where
  ?tolerance = Length.meters 1e-9

main :: IO ()
main = Task.main script
