module Main (main) where

import Angle qualified
import Arc2d qualified
import Area qualified
import Axis2d qualified
import Console qualified
import Curve2d qualified
import Debug qualified
import Direction2d qualified
import Direction3d ()
import Float qualified
import Length (Length)
import Length qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Parameter1d qualified
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import Random qualified
import Range qualified
import Result qualified
import Task qualified
import Text qualified
import Transform2d qualified
import Try qualified
import Units (Meters)
import Vector2d qualified
import Vector3d qualified
import Volume qualified

log :: Show a => Text -> a -> Task Text ()
log label value = Console.printLine (label ++ ": " ++ Debug.show value)

data WorldSpace

type WorldCoordinates = WorldSpace @ Meters

data MyPoints = MyPoints (Point2d WorldCoordinates) (Point2d WorldCoordinates) deriving (Show)

defaultTolerance :: Length
defaultTolerance = Length.meters 1e-9

offsetPoint
  :: Tolerance units
  => Point2d (space @ units)
  -> Point2d (space @ units)
  -> Qty units
  -> Point2d (space @ units)
offsetPoint startPoint endPoint distance = Result.withDefault startPoint do
  direction <- Direction2d.from startPoint endPoint
  Ok (Point2d.midpoint startPoint endPoint + distance * Direction2d.perpendicularTo direction)

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
  let testArc angle =
        Arc2d.with
          [ Arc2d.StartPoint Point2d.origin
          , Arc2d.EndPoint (Point2d.meters 1.0 1.0)
          , Arc2d.SweptAngle angle
          ]
  arc1 <- testArc (Angle.degrees 90.0)
  arc2 <- testArc (Angle.degrees -90.0)
  arc3 <- testArc (Angle.degrees 180.0)
  arc4 <- testArc (Angle.degrees -180.0)
  line <- testArc Qty.zero
  log "arc1 point" (Curve2d.evaluateAt 0.5 arc1)
  log "arc2 point" (Curve2d.evaluateAt 0.5 arc2)
  log "arc3 point" (Curve2d.evaluateAt 0.5 arc3)
  log "arc4 point" (Curve2d.evaluateAt 0.5 arc4)
  log "line point" (Curve2d.evaluateAt 0.5 line)
 where
  ?tolerance = defaultTolerance

getCrossProduct :: Result Text Float
getCrossProduct = Try.withContext "In getCrossProduct" Try.do
  vectorDirection <- Vector2d.direction (Vector2d.meters 2.0 3.0)
  lineDirection <-
    Try.withContext "When getting line direction" $
      Direction2d.from Point2d.origin Point2d.origin
  Ok (vectorDirection >< lineDirection)
 where
  ?tolerance = defaultTolerance

testTry :: Task Text ()
testTry =
  case Try.withContext "In testTry" getCrossProduct of
    Ok crossProduct -> log "Got cross product" crossProduct
    Error message -> Console.printLine message

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

testNonEmpty :: (Ord a, Show a) => List a -> Task Text ()
testNonEmpty [] = Console.printLine "List is empty"
testNonEmpty (NonEmpty nonEmpty) =
  Console.printLine ("List is non-empty, maximum is " ++ Debug.show (NonEmpty.maximum nonEmpty))

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
  log "sqrt 2.0" (Qty.sqrt 2.0)
  log "Equality test" (let ?tolerance = Length.centimeter in Length.meters 1.0 ~= Length.meters 1.005)
  testDirection2dAngleFrom
  testArc2dFrom
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
  log "Axis2d.x.originPoint" Axis2d.x.originPoint
  testTaskIteration
  testTaskSequencing
  log "Successive deltas" (List.successive subtract [0, 1, 4, 9, 16, 25])
  log "Successive intervals" (List.successive Range.from [1.0, 2.0, 3.0, 4.0])
  log "Prepend Maybe to List" (Just 1 ++ [2, 3])
  log "Offset point" (offsetPoint (Point2d.meters 1.0 0.0) (Point2d.meters 3.0 0.0) (Length.meters 1.0))
  testParameter1dGeneration
  testRangeFind
  testNonEmpty ([] :: List Int)
  testNonEmpty [2, 3, 1]
 where
  ?tolerance = defaultTolerance

main :: IO ()
main = Task.toIO script
