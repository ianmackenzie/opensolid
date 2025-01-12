-- {-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-local-binds -Wno-unused-matches -Wno-unused-top-binds #-}

module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Area qualified as Area
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Color (Color)
import OpenSolid.Color qualified as Color
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Debug qualified as Debug
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Drawing2d qualified as Drawing2d
import OpenSolid.Duration (Duration)
import OpenSolid.Duration qualified as Duration
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Curve1d qualified as Expression.Curve1d
import OpenSolid.Float qualified as Float
import OpenSolid.IO qualified as IO
import OpenSolid.Int qualified as Int
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Polyline2d (Polyline2d (Polyline2d))
import OpenSolid.Polyline2d qualified as Polyline2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Random qualified as Random
import OpenSolid.Range qualified as Range
import OpenSolid.Result qualified as Result
import OpenSolid.Solve2d qualified as Solve2d
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction.Zeros qualified as SurfaceFunction.Zeros
import OpenSolid.SurfaceParameter (UvCoordinates, UvPoint, UvSpace)
import OpenSolid.SurfaceParameter qualified as SurfaceParameter
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units (Meters)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d
import OpenSolid.Volume qualified as Volume

data Global deriving (Eq, Show)

log :: Show a => Text -> a -> IO ()
log label value = IO.printLine (label + ": " + Text.show value)

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
  let translatedPoint = Point2d.meters 2.0 3.0 + Vector2d.meters 4.0 5.0
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
testEquality = Tolerance.using Length.centimeter do
  log "Equality test" (Length.meters 1.0 ~= Length.meters 1.005)

testTransformation :: IO ()
testTransformation = IO.do
  log "Rotated axis" $
    (Axis2d.x |> Axis2d.rotateAround (Point2d.meters 1.0 0.0) Angle.quarterTurn)
  let originalPoints = [Point2d.meters 1.0 0.0, Point2d.meters 2.0 0.0, Point2d.meters 3.0 0.0]
  let rotationFunction = Point2d.rotateAround Point2d.origin Angle.quarterTurn
  let rotatedPoints = List.map rotationFunction originalPoints
  log "Rotated points" rotatedPoints
  let transformedAxis =
        Axis2d.x
          |> Axis2d.translateInOwn Axis2d.direction (Length.meters 2.0)
          |> Axis2d.rotateAroundOwn Axis2d.originPoint Angle.quarterTurn
  log "Transformed axis" transformedAxis

offsetPoint ::
  Tolerance units =>
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Qty units ->
  Point2d (space @ units)
offsetPoint startPoint endPoint distance =
  case Direction2d.from startPoint endPoint of
    Failure Direction2d.PointsAreCoincident -> startPoint
    Success direction -> do
      let displacement = distance * Direction2d.perpendicularTo direction
      Point2d.midpoint startPoint endPoint + displacement

testCustomFunction :: Tolerance Meters => IO ()
testCustomFunction =
  log "Offset point" $
    offsetPoint (Point2d.meters 1.0 0.0) (Point2d.meters 3.0 0.0) (Length.meters 1.0)

testListOperations :: IO ()
testListOperations = IO.do
  log "Successive deltas" (List.successive subtract [0, 1, 4, 9, 16, 25])
  log "Successive intervals" (List.successive Range.from [1.0, 2.0, 3.0, 4.0])
  log "Prepend Maybe to List" (Just 1 + [2, 3])

getCrossProduct :: Tolerance Meters => Result Text Float
getCrossProduct = Result.addContext "In getCrossProduct" Result.do
  vectorDirection <-
    Vector2d.direction (Vector2d.meters 2.0 3.0)
      |> Result.addContext "When getting vector direction"
  lineDirection <-
    Direction2d.from Point2d.origin Point2d.origin
      |> Result.addContext "When getting line direction"
  Success (vectorDirection >< lineDirection)

testTry :: Tolerance Meters => IO ()
testTry =
  IO.onError IO.printLine $
    IO.addContext "In testTry" IO.do
      crossProduct <- getCrossProduct
      log "Got cross product" crossProduct

testIOIteration :: IO ()
testIOIteration = IO.forEach [1 .. 3] (log "Looping")

doublingIO :: Text -> IO Int
doublingIO input = IO.do
  value <- Int.parse input
  let doubled = 2 * value
  IO.succeed doubled

doubleManyIO :: IO (List Int)
doubleManyIO = IO.collect doublingIO ["1", "-2", "3"]

testIOSequencing :: IO ()
testIOSequencing = IO.do
  doubledValues <- doubleManyIO
  IO.forEach doubledValues (log "Doubled value")

testParameter1dGeneration :: IO ()
testParameter1dGeneration = IO.do
  t1 <- Random.generate Parameter.random
  t2 <- Random.generate Parameter.random
  t3 <- Random.generate Parameter.random
  log "Random parameter value 1" t1
  log "Random parameter value 2" t2
  log "Random parameter value 3" t3

testEmptyCheck :: List Int -> IO ()
testEmptyCheck [] = IO.printLine "List is empty"
testEmptyCheck (NonEmpty nonEmpty) =
  IO.printLine ("List is non-empty, maximum is " + Text.int (NonEmpty.maximum nonEmpty))

testNonEmpty :: IO ()
testNonEmpty = IO.do
  testEmptyCheck []
  testEmptyCheck [2, 3, 1]

testPlaneTorusIntersection :: Tolerance Meters => IO ()
testPlaneTorusIntersection = IO.do
  let theta = Angle.twoPi * SurfaceFunction.u
  let phi = Angle.twoPi * SurfaceFunction.v
  let minorRadius = Length.centimeters 1.0
  let majorRadius = Length.centimeters 2.0
  let r = majorRadius + minorRadius * SurfaceFunction.cos phi
  let x = r * SurfaceFunction.cos theta
  let y = r * SurfaceFunction.sin theta
  let z = minorRadius * SurfaceFunction.sin phi
  let alpha = Angle.asin (minorRadius / majorRadius)
  let nx = -(Angle.sin alpha)
  let ny = 0.0
  let nz = Angle.cos alpha
  -- let nx = 1 / Float.sqrt 2.0
  -- let ny = 1 / Float.sqrt 2.0
  -- let nz = 0.0
  -- let nx = 0.0
  -- let ny = 0.0
  -- let nz = 1.0
  let f = x * nx + y * ny + z * nz
  -- let u = SurfaceFunction.u
  -- let v = SurfaceFunction.v
  -- let f = (SurfaceFunction.squared (u - 0.5) - SurfaceFunction.squared (v - 0.5)) * Length.meter
  zeros <- SurfaceFunction.zeros f
  drawZeros "executables/sandbox/test-plane-torus-intersection.svg" zeros
  IO.printLine "Plane torus intersection solutions:"
  log "  Crossing curves" (List.length (SurfaceFunction.Zeros.crossingCurves zeros))
  log "  Saddle points" (List.length (SurfaceFunction.Zeros.saddlePoints zeros))

testPlaneParaboloidIntersection :: IO ()
testPlaneParaboloidIntersection = Tolerance.using 1e-9 IO.do
  let u = SurfaceFunction.u
  let v = SurfaceFunction.v
  let f = SurfaceFunction.squared u + SurfaceFunction.squared v - 0.5
  zeros <- SurfaceFunction.zeros f
  drawZeros "executables/sandbox/test-plane-paraboloid-intersection.svg" zeros
  IO.printLine "Plane paraboloid intersection solutions:"
  log "  Crossing curves" (List.length (SurfaceFunction.Zeros.crossingCurves zeros))
  log "  Saddle points" (List.length (SurfaceFunction.Zeros.saddlePoints zeros))

strokeWidth :: Length
strokeWidth = Length.millimeters 0.1

drawZeros :: Text -> SurfaceFunction.Zeros -> IO ()
drawZeros path zeros = IO.do
  let uvRange = Range.convert toDrawing (Range.from -0.05 1.05)
  let viewBox = Bounds2d.xy uvRange uvRange
  let crossingCurves = SurfaceFunction.Zeros.crossingCurves zeros
  let saddlePoints = SurfaceFunction.Zeros.saddlePoints zeros
  Drawing2d.writeTo path viewBox $
    [ Drawing2d.with [Drawing2d.strokeWidth strokeWidth] $
        [ drawBounds [] (Bounds2d.convert toDrawing SurfaceParameter.domain)
        , Drawing2d.group (List.mapWithIndex drawCrossingCurve crossingCurves)
        , Drawing2d.group (List.map (drawDot Color.orange) saddlePoints)
        ]
    ]

drawBounds :: List (Drawing2d.Attribute space) -> Bounds2d (space @ Meters) -> Drawing2d.Entity space
drawBounds attributes bounds = do
  let point x y = Bounds2d.interpolate bounds x y
  Drawing2d.polygon attributes $
    [ point 0.0 0.0
    , point 1.0 0.0
    , point 1.0 1.0
    , point 0.0 1.0
    ]

drawCrossingCurve :: Int -> Curve2d UvCoordinates -> Drawing2d.Entity UvSpace
drawCrossingCurve index curve = do
  let hue = (Float.int index * Angle.goldenAngle) % Angle.twoPi
  let color = Color.hsl hue 0.5 0.5
  drawUvCurve [Drawing2d.strokeColor color] curve

toDrawing :: Qty (Meters :/: Unitless)
toDrawing = Length.centimeters 10.0 ./. 1.0

drawUvCurve :: List (Drawing2d.Attribute UvSpace) -> Curve2d UvCoordinates -> Drawing2d.Entity UvSpace
drawUvCurve attributes curve = do
  let polyline = Curve2d.toPolyline 0.001 (Curve2d.evaluate curve) curve
  Drawing2d.polyline attributes (Polyline2d.map (Point2d.convert toDrawing) polyline)

drawDot :: Color -> UvPoint -> Drawing2d.Entity UvSpace
drawDot color point =
  Drawing2d.circle
    [Drawing2d.fillColor color]
    (Length.millimeters 0.5)
    (Point2d.convert toDrawing point)

delayedPrint :: Int -> Duration -> IO ()
delayedPrint number delay = IO.do
  IO.sleep delay
  IO.printLine (Text.int number)

testConcurrency :: IO ()
testConcurrency = IO.do
  IO.printLine "Starting concurrency test..."
  IO.printLine "0"
  print5 <- IO.async (delayedPrint 5 (Duration.milliseconds 250.0))
  print2 <- IO.async (delayedPrint 2 (Duration.milliseconds 100.0))
  print3 <- IO.async (delayedPrint 3 (Duration.milliseconds 150.0))
  print4 <- IO.async (delayedPrint 4 (Duration.milliseconds 200.0))
  print1 <- IO.async (delayedPrint 1 (Duration.milliseconds 50.0))
  IO.await print4
  IO.await print2
  IO.await print3
  IO.await print5
  IO.await print1
  IO.printLine "Concurrency test complete!"

computeSquareRoot :: Float -> IO Float
computeSquareRoot value = IO.do
  IO.sleep (Duration.milliseconds 100.0)
  Success (Float.sqrt value)

testIOParallel :: IO ()
testIOParallel = IO.do
  IO.printLine "Computing square roots with IO.parallel"
  let values = List.map Float.int [0 .. 9]
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
  Color ->
  Point2d (space @ Unitless) ->
  List (Point2d (space @ Unitless)) ->
  Point2d (space @ Unitless) ->
  Drawing2d.Entity space
drawBezier color startPoint innerControlPoints endPoint = do
  let drawingStartPoint = Point2d.convert toDrawing startPoint
  let drawingEndPoint = Point2d.convert toDrawing endPoint
  let drawingInnerControlPoints = List.map (Point2d.convert toDrawing) innerControlPoints
  let drawingControlPoints = drawingStartPoint :| (drawingInnerControlPoints + [drawingEndPoint])
  let curve = Curve2d.bezier drawingControlPoints
  let drawSegmentBounds tRange = drawBounds [] (Curve2d.evaluateBounds curve tRange)
  Drawing2d.with
    [Drawing2d.strokeColor color, Drawing2d.strokeWidth (Length.millimeters 1.0)]
    [ Drawing2d.with [Drawing2d.opacity 0.3] $
        [ Drawing2d.polyline [] (Polyline2d drawingControlPoints)
        , Drawing2d.with [Drawing2d.fillColor color] $
            [ Drawing2d.circle [] (Length.millimeters 5.0) point
            | point <- NonEmpty.toList drawingControlPoints
            ]
        ]
    , Drawing2d.with [Drawing2d.opacity 0.3] $
        List.map drawSegmentBounds (Parameter.intervals 10)
    , Drawing2d.curve [] Length.millimeter curve
    ]

testBezierSegment :: Tolerance Meters => IO ()
testBezierSegment = IO.do
  let p1 = Point2d.origin @Global
  let p2 = Point2d.xy 0.0 5.0
  let p3 = Point2d.xy 2.5 10.0
  let p4 = Point2d.xy 5.0 0.0
  let p5 = Point2d.xy 10.0 5.0
  let p6 = Point2d.xy 10.0 10.0
  let coordinateRange = Range.convert toDrawing (Range.from -1.0 11.0)
  let drawingBounds = Bounds2d.xy coordinateRange coordinateRange
  let curveEntity = drawBezier Color.blue p1 [p2, p3, p4, p5] p6
  Drawing2d.writeTo "executables/sandbox/test-bezier-segment.svg" drawingBounds [curveEntity]

testHermiteBezier :: IO ()
testHermiteBezier = IO.do
  let startPoint = Point2d.origin @Global
  let startDerivatives = [Vector2d.meters 10.0 10.0]
  let endDerivatives = [Vector2d.meters 0.0 -10.0, Vector2d.zero]
  let endPoint = Point2d.meters 10.0 0.0
  let curve = Curve2d.hermite (startPoint, startDerivatives) (endPoint, endDerivatives)
  log "Hermite Bezier curve" curve
  let curveAttributes =
        [ Drawing2d.strokeColor Color.blue
        , Drawing2d.strokeWidth (Length.centimeters 3.0)
        ]
  let curveEntity = Drawing2d.curve curveAttributes Length.millimeter curve
  let coordinateRange = Range.from (Length.meters -1.0) (Length.meters 11.0)
  let drawingBounds = Bounds2d.xy coordinateRange coordinateRange
  Drawing2d.writeTo "executables/sandbox/test-hermite-bezier.svg" drawingBounds [curveEntity]

testExplicitRandomStep :: IO ()
testExplicitRandomStep = IO.do
  let seed0 = Random.init 1234
  let list0 = [5 .. 10]
  let generator = List.shuffle list0
  let (list1, seed1) = Random.step generator seed0
  log "list1" list1
  let (list2, seed2) = Random.step generator seed1
  log "list2" list2
  let (list3, _) = Random.step generator seed2
  log "list3" list3

testDebugPrint :: IO ()
testDebugPrint = do
  let xs = Text.repeat 2 "x"
  Debug.log "xs" xs
  let ys = Text.repeat 3 "y"
  Debug.log "ys" ys
  IO.printLine (xs + ys)

textSum :: Text -> Text -> Result Text Int
textSum t1 t2 = Result.do
  n1 <- Int.parse t1
  Debug.log "n1" n1
  n2 <- Int.parse t2
  Debug.log "n2" n2
  Success (n1 + n2)

testTextSum :: IO ()
testTextSum = IO.do
  IO.onError IO.printLine IO.do
    sum <- textSum "5" "abc"
    log "sum" sum
  IO.onError IO.printLine IO.do
    sum <- textSum "2" "3"
    log "sum" sum

testNewtonRaphson2d :: IO ()
testNewtonRaphson2d = Tolerance.using 1e-9 do
  let u = SurfaceFunction.u
  let v = SurfaceFunction.v
  let f = SurfaceFunction.squared u + SurfaceFunction.squared v - 4.0
  let g = u - v
  let bounds = Bounds2d.xy (Range.from 0.0 2.0) (Range.from 0.0 2.0)
  let function = VectorSurfaceFunction2d.xy f g
  let solution =
        Solve2d.unique
          (VectorSurfaceFunction2d.evaluateBounds function)
          (VectorSurfaceFunction2d.evaluate function)
          (VectorSurfaceFunction2d.evaluate (VectorSurfaceFunction2d.derivative SurfaceParameter.U function))
          (VectorSurfaceFunction2d.evaluate (VectorSurfaceFunction2d.derivative SurfaceParameter.V function))
          bounds
  log "Solve2d.unique solution" solution

testJit :: IO ()
testJit = IO.do
  let x = Expression.t
  let xSquared = Expression.squared x
  let function = xSquared / (xSquared + Expression.Curve1d.constant 1.0)
  log "JIT evaluated" (Expression.evaluate function 2.0)
  log "JIT bounds" (Expression.evaluateBounds function (Range.from 1.0 3.0))

testJitCurve2d :: IO ()
testJitCurve2d = IO.do
  let x = Expression.Curve1d.constant 10.0 * Expression.t
  let y = Expression.sqrt Expression.t
  let curve = Expression.xy x y :: Expression Float (Point2d (Global @ Unitless))
  log "Evaluated 2D curve" (Expression.evaluate curve 3.0)

testQuadraticSplineExpression :: IO ()
testQuadraticSplineExpression = IO.do
  let p1 = Point2d.meters 1.0 2.0
  let p2 = Point2d.meters 3.0 4.0
  let p3 = Point2d.meters 5.0 6.0
  let spline = Curve2d.quadraticBezier p1 p2 p3
  log "spline" spline
  log "spline.x" (Curve2d.xCoordinate spline)

main :: IO ()
main = Tolerance.using (Length.meters 1e-9) IO.do
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
  testBezierSegment
  testHermiteBezier
  testJit
  testJitCurve2d
  testPlaneParaboloidIntersection
  testPlaneTorusIntersection
  testExplicitRandomStep
  testConcurrency
  testIOParallel
  testParallelComputation
  testDebugPrint
  testTextSum
  testNewtonRaphson2d
  testQuadraticSplineExpression
