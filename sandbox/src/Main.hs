module Main (main) where

import Angle qualified
import Arc2d qualified
import Area qualified
import Axis2d qualified
import BezierCurve2d qualified
import Bounds2d (Bounds2d (Bounds2d))
import Bounds2d qualified
import Colour (Colour)
import Colour qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Debug qualified
import Direction2d qualified
import Direction3d ()
import Drawing2d qualified
import Duration qualified
import Error qualified
import Float qualified
import IO qualified
import Int qualified
import Length (Length)
import Length qualified
import Line2d qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Parameter qualified
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import Random qualified
import Range qualified
import Result qualified
import String qualified
import Surface1d.Function qualified
import Surface1d.Function.Zeros qualified
import Tolerance qualified
import Transform2d qualified
import Units (Meters)
import Uv (Parameter (U, V))
import Uv qualified
import Vector2d qualified
import Vector3d qualified
import VectorCurve2d qualified
import Volume qualified

log :: Show a => String -> a -> IO ()
log label value = IO.printLine (label + ": " + show value)

testScalarArithmetic :: IO ()
testScalarArithmetic = IO.do
  log "Integer product" (3 * 4)
  log "Integer division" (10 // 4)
  log "True division" (10 / 4)
  let area = Area.squareMeters 3.0
  let length = Length.centimeters 3.0
  let volume = area * length
  let volumeInCubicCentimeters = Volume.inCubicCentimeters volume
  log "Volume in cubic centimeters" volumeInCubicCentimeters
  log "sqrt 2.0" (Qty.sqrt 2.0)

testVectorArithmetic :: IO ()
testVectorArithmetic = IO.do
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

testRangeArithmetic :: IO ()
testRangeArithmetic = IO.do
  let rangeDifference = Range.from (Length.meters 2.0) (Length.meters 3.0) - Length.centimeters 50.0
  log "Range difference" rangeDifference
  let rangeProduct = Length.centimeters 20.0 * Range.from (Length.meters 2.0) (Length.meters 3.0)
  log "Range product" rangeProduct

testEquality :: IO ()
testEquality =
  log "Equality test" (Tolerance.using Length.centimeter (Length.meters 1.0 ~= Length.meters 1.005))

testTransformation :: IO ()
testTransformation = IO.do
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
  Tolerance units =>
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Qty units ->
  Point2d (space @ units)
offsetPoint startPoint endPoint distance = Result.withDefault startPoint Result.do
  direction <- Direction2d.from startPoint endPoint
  Ok (Point2d.midpoint startPoint endPoint + distance * Direction2d.perpendicularTo direction)

testCustomFunction :: Tolerance Meters => IO ()
testCustomFunction =
  log "Offset point" $
    offsetPoint (Point2d.meters 1.0 0.0) (Point2d.meters 3.0 0.0) (Length.meters 1.0)

testListOperations :: IO ()
testListOperations = IO.do
  log "Successive deltas" (List.successive subtract [0, 1, 4, 9, 16, 25])
  log "Successive intervals" (List.successive Range.from [1.0, 2.0, 3.0, 4.0])
  log "Prepend Maybe to List" (Just 1 + [2, 3])

getCrossProduct :: Tolerance Meters => Result String Float
getCrossProduct = Error.context "In getCrossProduct" Result.do
  vectorDirection <-
    Vector2d.direction (Vector2d.meters 2.0 3.0)
      |> Error.debug (\_ -> IO.printLine "Couldn't get vector direction!")
      |> Error.context "When getting vector direction"
  lineDirection <-
    Direction2d.from Point2d.origin Point2d.origin
      |> Error.debug (\_ -> IO.printLine "Couldn't get line direction!")
      |> Error.context "When getting line direction"
  Ok (vectorDirection >< lineDirection)

testTry :: Tolerance Meters => IO ()
testTry =
  IO.onError IO.printLine $
    Error.context "In testTry" IO.do
      crossProduct <- getCrossProduct
      log "Got cross product" crossProduct

testIOIteration :: IO ()
testIOIteration = IO.forEach [1 .. 3] (log "Looping")

doublingIO :: String -> IO Int
doublingIO input = IO.do
  value <- Int.parse input
  let doubled = 2 * value
  IO.return doubled

doubleManyIO :: IO (List Int)
doubleManyIO = IO.collect doublingIO ["1", "-2", "3"]

testIOSequencing :: IO ()
testIOSequencing = IO.do
  doubledValues <- doubleManyIO
  IO.forEach doubledValues (log "Doubled value")

testParameter1dGeneration :: IO ()
testParameter1dGeneration = IO.do
  t1 <- Random.generate Parameter.generator
  t2 <- Random.generate Parameter.generator
  t3 <- Random.generate Parameter.generator
  log "Random parameter value 1" t1
  log "Random parameter value 2" t2
  log "Random parameter value 3" t3

testEmptyCheck :: List Int -> IO ()
testEmptyCheck [] = IO.printLine "List is empty"
testEmptyCheck (NonEmpty nonEmpty) =
  IO.printLine ("List is non-empty, maximum is " + show (NonEmpty.maximum nonEmpty))

testNonEmpty :: IO ()
testNonEmpty = IO.do
  testEmptyCheck []
  testEmptyCheck [2, 3, 1]

testLineFromEndpoints :: Tolerance Meters => IO ()
testLineFromEndpoints = IO.do
  line1 <-
    Line2d.build
      ( Line2d.startPoint Point2d.origin
      , Line2d.endPoint (Point2d.centimeters 40.0 30.0)
      )
  case line1 of
    Curve2d.Line{length} ->
      log "Line length in centimeters" (Length.inCentimeters length)
    _ -> log "Unexpected curve" line1

testDirectedLine :: Tolerance Meters => IO ()
testDirectedLine = IO.do
  let line = Line2d.directed Point2d.origin (Direction2d.degrees 45.0) (Length.meters 2.0)
  log "Line end point" (Curve2d.endPoint line)

testArcFromEndpoints :: Tolerance Meters => IO ()
testArcFromEndpoints = IO.do
  arc <-
    Arc2d.with
      ( Arc2d.startPoint Point2d.origin
      , Arc2d.endPoint (Point2d.centimeters 50.0 50.0)
      , Arc2d.sweptAngle Angle.quarterTurn
      )
  case arc of
    Curve2d.Arc{centerPoint} -> log "Arc center point" centerPoint
    _ -> log "Unexpected curve" arc

testPlaneTorusIntersection :: Tolerance Meters => IO ()
testPlaneTorusIntersection = IO.do
  let theta = Angle.twoPi * Surface1d.Function.parameter U
  let phi = Angle.twoPi * Surface1d.Function.parameter V
  let minorRadius = Length.centimeters 1.0
  let majorRadius = Length.centimeters 2.0
  let r = majorRadius + minorRadius * Surface1d.Function.cos phi
  let x = r * Surface1d.Function.cos theta
  let y = r * Surface1d.Function.sin theta
  let z = minorRadius * Surface1d.Function.sin phi
  let alpha = Angle.asin (minorRadius / majorRadius)
  let nx = -(Angle.sin alpha)
  let ny = 0.0
  let nz = Angle.cos alpha
  -- let nx = 1.0 / Float.sqrt 2.0
  -- let ny = 1.0 / Float.sqrt 2.0
  -- let nz = 0.0
  -- let nx = 0.0
  -- let ny = 0.0
  -- let nz = 1.0
  let f = x * nx + y * ny + z * nz
  zeros <- Surface1d.Function.zeros f
  drawZeros "test-plane-torus-intersection.svg" zeros
  IO.printLine "Plane torus intersection solutions:"
  log "  Crossing curves" (List.length (Surface1d.Function.Zeros.crossingCurves zeros))
  log "  Saddle points" (List.length (Surface1d.Function.Zeros.saddlePoints zeros))

strokeWidth :: Length
strokeWidth = Length.millimeters 0.1

drawZeros :: String -> Surface1d.Function.Zeros.Zeros -> IO ()
drawZeros path zeros = IO.do
  let uvRange = Range.convert toDrawing (Range.from -0.05 1.05)
  let viewBox = Bounds2d uvRange uvRange
  Drawing2d.writeTo path viewBox $
    [ Drawing2d.with [Drawing2d.strokeWidth strokeWidth] $
        [ drawBounds [] Uv.domain
        , Drawing2d.group (List.mapWithIndex drawCrossingCurve (Surface1d.Function.Zeros.crossingCurves zeros))
        , Drawing2d.group (List.map drawSaddlePoint (Surface1d.Function.Zeros.saddlePoints zeros))
        ]
    ]

drawBounds :: List (Drawing2d.Attribute Uv.Space) -> Uv.Bounds -> Drawing2d.Entity Uv.Space
drawBounds attributes bounds = do
  let point x y = Point2d.convert toDrawing (Bounds2d.interpolate bounds x y)
  Drawing2d.polygon attributes $
    [ point 0.0 0.0
    , point 1.0 0.0
    , point 1.0 1.0
    , point 0.0 1.0
    ]

drawCrossingCurve :: Int -> NonEmpty (Curve2d Uv.Coordinates) -> Drawing2d.Entity Uv.Space
drawCrossingCurve index segments = do
  let hue = (Float.fromInt index * Angle.goldenAngle) % Angle.twoPi
  let colour = Colour.hsl hue 0.5 0.5
  Drawing2d.with [Drawing2d.strokeColour colour, Drawing2d.opacity 0.3] $
    List.map drawCurve (NonEmpty.toList segments)

drawSaddlePoint :: Uv.Point -> Drawing2d.Entity Uv.Space
drawSaddlePoint point = drawDot Colour.orange point

toDrawing :: Qty (Meters :/: Unitless)
toDrawing = Length.centimeters 10.0 ./. 1.0

drawCurve :: Curve2d Uv.Coordinates -> Drawing2d.Entity Uv.Space
drawCurve curve = do
  let pointOnCurve t = Point2d.convert toDrawing (Curve2d.pointOn curve t)
  let sampledPoints = List.map pointOnCurve (Parameter.steps 20)
  Drawing2d.polyline [] sampledPoints

drawDot :: Colour -> Uv.Point -> Drawing2d.Entity Uv.Space
drawDot colour point =
  Drawing2d.circle
    [Drawing2d.fillColour colour]
    (Point2d.convert toDrawing point)
    (Length.millimeters 0.5)

delayedPrint :: Int -> IO ()
delayedPrint numSeconds = IO.do
  IO.sleep (Duration.seconds (Float.fromInt numSeconds))
  IO.printLine (String.fromInt numSeconds)

testConcurrency :: IO ()
testConcurrency = IO.do
  IO.printLine "Starting concurrency test..."
  IO.printLine "0"
  print5 <- IO.async (delayedPrint 5)
  print2 <- IO.async (delayedPrint 2)
  print3 <- IO.async (delayedPrint 3)
  print4 <- IO.async (delayedPrint 4)
  print1 <- IO.async (delayedPrint 1)
  IO.await print4
  IO.await print2
  IO.await print3
  IO.await print5
  IO.await print1
  IO.printLine "Concurrency test complete!"

computeSquareRoot :: Float -> IO Float
computeSquareRoot value = IO.do
  IO.sleep Duration.second
  IO.return (Float.sqrt value)

testIOParallel :: IO ()
testIOParallel = IO.do
  IO.printLine "Computing square roots with IO.parallel"
  let values = List.map Float.fromInt [0 .. 9]
  squareRoots <- IO.parallel computeSquareRoot values
  log "Square roots" squareRoots

testParallelComputation :: IO ()
testParallelComputation = IO.do
  computeSqrt1 <- IO.async (computeSquareRoot 1.0)
  computeSqrt4 <- IO.async (computeSquareRoot 4.0)
  computeSqrt9 <- IO.async (computeSquareRoot 9.0)
  computeSqrt16 <- IO.async (computeSquareRoot 16.0)
  computeSqrt25 <- IO.async (computeSquareRoot 25.0)
  sqrt1 <- IO.await computeSqrt1
  sqrt4 <- IO.await computeSqrt4
  sqrt9 <- IO.await computeSqrt9
  sqrt16 <- IO.await computeSqrt16
  sqrt25 <- IO.await computeSqrt25
  log "sqrt1" sqrt1
  log "sqrt4" sqrt4
  log "sqrt9" sqrt9
  log "sqrt16" sqrt16
  log "sqrt25" sqrt25

drawBezier ::
  Tolerance Meters =>
  Colour ->
  Point2d (space @ Unitless) ->
  List (Point2d (space @ Unitless)) ->
  Point2d (space @ Unitless) ->
  Result Curve2d.DegenerateCurve (Drawing2d.Entity space)
drawBezier colour startPoint innerControlPoints endPoint = Result.do
  let drawingStartPoint = Point2d.convert toDrawing startPoint
  let drawingEndPoint = Point2d.convert toDrawing endPoint
  let drawingInnerControlPoints = List.map (Point2d.convert toDrawing) innerControlPoints
  let drawingControlPoints = List.concat [[drawingStartPoint], drawingInnerControlPoints, [drawingEndPoint]]
  curve <- BezierCurve2d.fromControlPoints drawingStartPoint drawingInnerControlPoints drawingEndPoint
  Ok $
    Drawing2d.with
      [Drawing2d.strokeColour colour, Drawing2d.strokeWidth (Length.millimeters 1.0)]
      [ Drawing2d.with [Drawing2d.opacity 0.3] $
          [ Drawing2d.polyline [] drawingControlPoints
          , Drawing2d.with [Drawing2d.fillColour colour] $
              [ Drawing2d.circle [] point (Length.millimeters 5.0)
              | point <- drawingControlPoints
              ]
          ]
      , Drawing2d.polyline [] $
          [ Curve2d.evaluateAt t curve
          | t <- Parameter.steps 100
          ]
      ]

testBezierSegment :: Tolerance Meters => IO ()
testBezierSegment = IO.do
  let p1 = Point2d.xy 0.0 0.0
  let p2 = Point2d.xy 0.0 5.0
  let p3 = Point2d.xy 2.5 10.0
  let p4 = Point2d.xy 5.0 0.0
  let p5 = Point2d.xy 10.0 5.0
  let p6 = Point2d.xy 10.0 10.0
  let drawingBounds = Bounds2d.convert toDrawing (Bounds2d (Range.from -1.0 11.0) (Range.from -1.0 11.0))
  curveEntity <- drawBezier Colour.blue p1 [p2, p3, p4, p5] p6
  Drawing2d.writeTo "test-bezier-segment.svg" drawingBounds [curveEntity]

testHermiteBezier :: Tolerance Meters => IO ()
testHermiteBezier = IO.do
  let startPoint = Point2d.origin
  let startDerivatives = [Vector2d.meters 10.0 10.0]
  let endDerivatives = [Vector2d.meters 0.0 -10.0, Vector2d.zero]
  let endPoint = Point2d.meters 10.0 0.0
  curve <- BezierCurve2d.hermite (startPoint, startDerivatives) (endPoint, endDerivatives)
  let curveFirstDerivative = Curve2d.derivative curve
  let curveSecondDerivative = VectorCurve2d.derivative curveFirstDerivative
  let curveThirdDerivative = VectorCurve2d.derivative curveSecondDerivative
  log "Start first derivative" (VectorCurve2d.evaluateAt 0.0 curveFirstDerivative)
  log "Start second derivative" (VectorCurve2d.evaluateAt 0.0 curveSecondDerivative)
  log "Start third derivative" (VectorCurve2d.evaluateAt 0.0 curveThirdDerivative)
  log "End first derivative" (VectorCurve2d.evaluateAt 1.0 curveFirstDerivative)
  log "End second derivative" (VectorCurve2d.evaluateAt 1.0 curveSecondDerivative)
  log "End third derivative" (VectorCurve2d.evaluateAt 1.0 curveThirdDerivative)
  let sampledPoints = [Curve2d.evaluateAt t curve | t <- Parameter.steps 100]
  let curveAttributes =
        [ Drawing2d.strokeColour Colour.blue
        , Drawing2d.strokeWidth (Length.centimeters 3.0)
        ]
  let curveEntity = Drawing2d.polyline curveAttributes sampledPoints
  let coordinateRange = Range.from (Length.meters -1.0) (Length.meters 11.0)
  let drawingBounds = Bounds2d coordinateRange coordinateRange
  Drawing2d.writeTo "test-hermite-bezier.svg" drawingBounds [curveEntity]

testDebugPrint :: IO ()
testDebugPrint = do
  let xs = String.repeat 2 "x"
  Debug.log "xs" xs
  let ys = String.repeat 3 "y"
  Debug.log "ys" ys
  IO.printLine (xs + ys)

stringSum :: String -> String -> Result String Int
stringSum s1 s2 = Result.do
  n1 <- Int.parse s1
  Debug.log "n1" n1
  n2 <- Int.parse s2
  Debug.log "n2" n2
  Ok (n1 + n2)

testStringSum :: IO ()
testStringSum = IO.do
  IO.onError IO.printLine IO.do
    sum <- stringSum "5" "abc"
    log "sum" sum
  IO.onError IO.printLine IO.do
    sum <- stringSum "2" "3"
    log "sum" sum

main :: IO ()
main = IO.do
  testScalarArithmetic
  testVectorArithmetic
  testRangeArithmetic
  testEquality
  testTransformation
  testTry
  testIOIteration
  testIOSequencing
  testCustomFunction
  testListOperations
  testParameter1dGeneration
  testNonEmpty
  testLineFromEndpoints
  testDirectedLine
  testArcFromEndpoints
  testBezierSegment
  testHermiteBezier
  testPlaneTorusIntersection
  testConcurrency
  testIOParallel
  testParallelComputation
  testDebugPrint
  testStringSum
 where
  ?tolerance = Length.meters 1e-9
