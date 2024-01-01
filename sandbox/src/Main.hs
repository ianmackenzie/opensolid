module Main (main) where

import Angle qualified
import Area qualified
import Axis2d qualified
import Bounds2d (Bounds2d (Bounds2d))
import Console qualified
import Curve2d qualified
import Direction2d qualified
import Direction3d ()
import Length qualified
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
import Task qualified
import Transform2d qualified
import Try qualified
import U qualified
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
  u1 <- Random.generate U.generator
  u2 <- Random.generate U.generator
  u3 <- Random.generate U.generator
  log "Random parameter value 1" u1
  log "Random parameter value 2" u2
  log "Random parameter value 3" u3

testEmptyCheck :: List Int -> Task String ()
testEmptyCheck [] = Console.printLine "List is empty"
testEmptyCheck (NonEmpty nonEmpty) =
  Console.printLine ("List is non-empty, maximum is " ++ show (NonEmpty.maximum nonEmpty))

testNonEmpty :: Task String ()
testNonEmpty = Try.do
  testEmptyCheck []
  testEmptyCheck [2, 3, 1]

testSurface1dIntersection :: Task String ()
testSurface1dIntersection = Try.do
  let u = Surface1d.Function.parameter U
  let v = Surface1d.Function.parameter V
  let f = Surface1d.Function.squared u + Surface1d.Function.squared v - 1.0
  let fu = f |> Surface1d.Function.derivative U
  let fv = f |> Surface1d.Function.derivative V
  let fuu = fu |> Surface1d.Function.derivative U
  let fvv = fv |> Surface1d.Function.derivative V
  let fuv = fu |> Surface1d.Function.derivative V
  let uvBounds = Bounds2d (Range.from -2.0 2.0) (Range.from -2.0 2.0)
  (solutions, _) <- Task.evaluate (Surface1d.Function.findSolutions f fu fv fuu fvv fuv uvBounds U [])
  log "Number of solutions" (List.length solutions)
  Task.forEach solutions $ \solution ->
    case solution of
      Surface1d.Solution.CrossingLoop {segments} -> do
        log "Crossing loop with size" (NonEmpty.length segments)
        Task.forEach (NonEmpty.toList segments) $ \curve -> do
          let Point2d x y = Curve2d.startPoint curve
          Console.printLine (String.fromFloat x ++ "," ++ String.fromFloat y)
      Surface1d.Solution.CrossingCurve {segments} ->
        log "Extra crossing curve with size" (NonEmpty.length segments)
      _ -> log "  Unexpected solution" solution
 where
  ?tolerance = 1e-9

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
 where
  ?tolerance = Length.meters 1e-9

main :: IO ()
main = Task.main script
