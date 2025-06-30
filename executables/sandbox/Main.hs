-- {-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-local-binds -Wno-unused-matches -Wno-unused-top-binds #-}

module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Area qualified as Area
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Color (Color)
import OpenSolid.Color qualified as Color
import OpenSolid.Convention3d qualified as Convention3d
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Debug qualified as Debug
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Drawing2d (Drawing2d)
import OpenSolid.Drawing2d qualified as Drawing2d
import OpenSolid.Duration (Duration)
import OpenSolid.Duration qualified as Duration
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Curve1d qualified as Expression.Curve1d
import OpenSolid.Float qualified as Float
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.IO qualified as IO
import OpenSolid.IO.Parallel qualified as IO.Parallel
import OpenSolid.Int qualified as Int
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Polyline2d (Polyline2d (Polyline2d))
import OpenSolid.Polyline2d qualified as Polyline2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Random qualified as Random
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Solve2d qualified as Solve2d
import OpenSolid.Surface3d qualified as Surface3d
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction.Zeros qualified as SurfaceFunction.Zeros
import OpenSolid.SurfaceParameter qualified as SurfaceParameter
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Try qualified as Try
import OpenSolid.UvBounds qualified as UvBounds
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d
import OpenSolid.Volume qualified as Volume

data Global deriving (Eq, Show)

log :: Show a => Text -> a -> IO ()
log label value = IO.printLine (label <> ": " <> Text.show value)

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
  let vector x y z =
        Vector3d.fromComponents Convention3d.zUp $
          (Length.meters x, Length.meters y, Length.meters z)
  let v1 = Vector2d.meters 1.0 2.0
  let v2 = 0.5 * Vector2d.meters 3.0 4.0
  let dotProduct = v1 `dot` v2
  log "Dot product" dotProduct
  log "2D cross product" (v1 `cross` v2)
  let squareRoot = Qty.sqrt dotProduct
  log "Square root" squareRoot
  let translatedPoint = Point2d.meters 2.0 3.0 + Vector2d.meters 4.0 5.0
  log "Translated point" translatedPoint
  let vectorSum = Vector2d.meters 1.0 2.0 + Vector2d.meters 2.0 3.0
  log "Vector sum" vectorSum
  let crossProduct = vector 1.0 2.0 3.0 `cross` vector 4.0 5.0 6.0
  log "Cross product" crossProduct
  let scaledVector = Length.meters 2.0 * Vector2d.meters 3.0 4.0
  log "Scaled vector" scaledVector

testBoundsArithmetic :: IO ()
testBoundsArithmetic = IO.do
  let boundsDifference = Bounds (Length.meters 2.0) (Length.meters 3.0) - Length.centimeters 50.0
  log "Bounds difference" boundsDifference
  let boundsProduct = Length.centimeters 20.0 * Bounds (Length.meters 2.0) (Length.meters 3.0)
  log "Bounds product" boundsProduct

testEquality :: IO ()
testEquality = Tolerance.using Length.centimeter do
  log "Equality test" (Length.meters 1.0 ~= Length.meters 1.005)

testTransformation :: IO ()
testTransformation = IO.do
  let rotatedAxis = Axis2d.x |> Axis2d.rotateAround (Point2d.meters 1.0 0.0) Angle.quarterTurn
  log "Rotated axis" rotatedAxis
  let originalPoints = [Point2d.meters 1.0 0.0, Point2d.meters 2.0 0.0, Point2d.meters 3.0 0.0]
  let rotationFunction = Point2d.rotateAround Point2d.origin Angle.quarterTurn
  let rotatedPoints = List.map rotationFunction originalPoints
  log "Rotated points" rotatedPoints

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
testCustomFunction = IO.do
  let point = offsetPoint (Point2d.meters 1.0 0.0) (Point2d.meters 3.0 0.0) (Length.meters 1.0)
  log "Offset point" point

testListOperations :: IO ()
testListOperations = IO.do
  let deltas = List.successive subtract [0, 1, 4, 9, 16, 25]
  let intervals = List.successive Bounds [1.0, 2.0, 3.0, 4.0]
  log "Successive deltas" deltas
  log "Successive intervals" intervals

getCrossProduct :: Tolerance Meters => Result Text Float
getCrossProduct = Result.addContext "In getCrossProduct" Try.do
  vectorDirection <-
    Vector2d.direction (Vector2d.meters 2.0 3.0)
      |> Result.addContext "When getting vector direction"
  lineDirection <-
    Direction2d.from Point2d.origin Point2d.origin
      |> Result.addContext "When getting line direction"
  Success (vectorDirection `cross` lineDirection)

testTry :: Tolerance Meters => IO ()
testTry =
  IO.onError IO.printLine do
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
  IO.printLine ("List is non-empty, maximum is " <> Text.int (NonEmpty.maximum nonEmpty))

testNonEmpty :: IO ()
testNonEmpty = IO.do
  testEmptyCheck []
  testEmptyCheck [2, 3, 1]

testPlaneTorusIntersection :: Tolerance Meters => IO ()
testPlaneTorusIntersection = IO.do
  let world = Frame3d.world
  let minorRadius = Length.centimeters 1.0
  let majorRadius = Length.centimeters 2.0
  let crossSection =
        Curve2d.circle do
          #centerPoint (Point2d.x majorRadius)
          #diameter (2.0 * minorRadius)
  surface <- Surface3d.revolved world.frontPlane crossSection Axis2d.y Angle.twoPi
  let alpha = Angle.asin (minorRadius / majorRadius)
  -- Other possibilities: Direction3d.xy (Angle.degrees 45), Direction3d.z
  let planeNormal = Direction3d.polar world.frontPlane (alpha + Angle.halfPi)
  let f = planeNormal `dot` (surface.function - world.originPoint)
  zeros <- SurfaceFunction.zeros f
  drawZeros "executables/sandbox/test-plane-torus-intersection.svg" zeros
  IO.printLine "Plane torus intersection solutions:"
  log "  Crossing curves" zeros.crossingCurves.length
  log "  Saddle points" zeros.saddlePoints.length

testPlaneParaboloidIntersection :: IO ()
testPlaneParaboloidIntersection = Tolerance.using 1e-9 IO.do
  let u = SurfaceFunction.u
  let v = SurfaceFunction.v
  let f = SurfaceFunction.squared u + SurfaceFunction.squared v - 0.5
  zeros <- SurfaceFunction.zeros f
  drawZeros "executables/sandbox/test-plane-paraboloid-intersection.svg" zeros
  IO.printLine "Plane paraboloid intersection solutions:"
  log "  Crossing curves" zeros.crossingCurves.length
  log "  Saddle points" zeros.saddlePoints.length

strokeWidth :: Length
strokeWidth = Length.millimeters 0.1

drawZeros :: Text -> SurfaceFunction.Zeros -> IO ()
drawZeros path zeros = IO.do
  let uvBounds = Bounds.convert toDrawing (Bounds -0.05 1.05)
  let viewBox = Bounds2d uvBounds uvBounds
  Drawing2d.writeSvg path viewBox $
    Drawing2d.groupWith [Drawing2d.strokeWidth strokeWidth] $
      [ drawBounds (Bounds2d.convert toDrawing UvBounds.unitSquare)
      , Drawing2d.group (List.mapWithIndex drawCrossingCurve zeros.crossingCurves)
      , Drawing2d.collect (drawDot Color.orange) zeros.saddlePoints
      ]

drawBounds :: Bounds2d (space @ Meters) -> Drawing2d space
drawBounds bounds = do
  let corner x y = Bounds2d.interpolate bounds x y
  Drawing2d.polygon [corner 0.0 0.0, corner 1.0 0.0, corner 1.0 1.0, corner 0.0 1.0]

drawCrossingCurve :: Int -> Curve2d UvCoordinates -> Drawing2d UvSpace
drawCrossingCurve index curve = do
  let hue = (Float.int index * Angle.goldenAngle) % Angle.twoPi
  let color = Color.hsl hue 0.5 0.5
  drawUvCurve [Drawing2d.strokeColor color] curve

toDrawing :: Qty (Meters :/: Unitless)
toDrawing = Length.centimeters 10.0 ./. 1.0

drawUvCurve :: List (Drawing2d.Attribute UvSpace) -> Curve2d UvCoordinates -> Drawing2d UvSpace
drawUvCurve attributes curve = do
  let resolution = Resolution.maxError 0.001
  let polyline = Curve2d.toPolyline resolution curve
  Drawing2d.polylineWith attributes (Polyline2d.map (Point2d.convert toDrawing) polyline)

drawDot :: Color -> UvPoint -> Drawing2d UvSpace
drawDot color point =
  Drawing2d.circleWith [Drawing2d.fillColor color] do
    #centerPoint (Point2d.convert toDrawing point)
    #diameter (Length.millimeters 1.0)

delayedPrint :: Int -> Duration -> IO ()
delayedPrint number delay = IO.do
  IO.sleep delay
  IO.printLine (Text.int number)

testConcurrency :: IO ()
testConcurrency = IO.do
  IO.printLine "Starting concurrency test..."
  IO.printLine "0"
  IO.Parallel.run
    [ delayedPrint 5 (Duration.milliseconds 250.0)
    , delayedPrint 2 (Duration.milliseconds 100.0)
    , delayedPrint 3 (Duration.milliseconds 150.0)
    , delayedPrint 4 (Duration.milliseconds 200.0)
    , delayedPrint 1 (Duration.milliseconds 50.0)
    ]
  IO.printLine "Concurrency test complete!"

computeSquareRoot :: Float -> IO Float
computeSquareRoot value = IO.do
  IO.sleep (Duration.milliseconds 100.0)
  Success (Float.sqrt value)

testIOParallel :: IO ()
testIOParallel = IO.do
  squareRoots <- IO.Parallel.collect computeSquareRoot [1.0, 4.0, 9.0, 16.0, 25.0]
  log "Square roots" squareRoots

drawBezier ::
  Tolerance Meters =>
  Color ->
  Point2d (space @ Unitless) ->
  List (Point2d (space @ Unitless)) ->
  Point2d (space @ Unitless) ->
  Drawing2d space
drawBezier color startPoint innerControlPoints endPoint = do
  let drawingStartPoint = Point2d.convert toDrawing startPoint
  let drawingEndPoint = Point2d.convert toDrawing endPoint
  let drawingInnerControlPoints = List.map (Point2d.convert toDrawing) innerControlPoints
  let drawingControlPoints = drawingStartPoint :| (drawingInnerControlPoints <> [drawingEndPoint])
  let curve = Curve2d.bezier drawingControlPoints
  let drawSegmentBounds tBounds = drawBounds (Curve2d.evaluateBounds curve tBounds)
  let controlPointDiameter = Length.millimeters 10.0
  let drawControlPoint point = Drawing2d.circle (#centerPoint point, #diameter controlPointDiameter)
  let resolution = Resolution.maxError Length.millimeter
  Drawing2d.groupWith
    [ Drawing2d.strokeColor color
    , Drawing2d.strokeWidth (Length.millimeters 1.0)
    ]
    [ Drawing2d.groupWith [Drawing2d.opacity 0.3] $
        [ Drawing2d.polyline (Polyline2d drawingControlPoints)
        , Drawing2d.collectWith [Drawing2d.fillColor color] drawControlPoint drawingControlPoints
        , Drawing2d.collect drawSegmentBounds (Parameter.intervals 10)
        ]
    , Drawing2d.curve resolution curve
    ]

testBezierSegment :: Tolerance Meters => IO ()
testBezierSegment = IO.do
  let p1 = Point2d.origin @Global
  let p2 = Point2d 0.0 5.0
  let p3 = Point2d 2.5 10.0
  let p4 = Point2d 5.0 0.0
  let p5 = Point2d 10.0 5.0
  let p6 = Point2d 10.0 10.0
  let coordinateBounds = Bounds.convert toDrawing (Bounds -1.0 11.0)
  let drawingBounds = Bounds2d coordinateBounds coordinateBounds
  let curveEntity = drawBezier Color.blue p1 [p2, p3, p4, p5] p6
  Drawing2d.writeSvg "executables/sandbox/test-bezier-segment.svg" drawingBounds curveEntity

testHermiteBezier :: IO ()
testHermiteBezier = IO.do
  let startPoint = Point2d.origin @Global
  let startDerivatives = [Vector2d.centimeters 10.0 10.0]
  let endDerivatives = [Vector2d.centimeters 0.0 -10.0, Vector2d.zero]
  let endPoint = Point2d.centimeters 10.0 0.0
  let curve = Curve2d.hermite startPoint startDerivatives endPoint endDerivatives
  let curveAttributes =
        [ Drawing2d.strokeColor Color.blue
        , Drawing2d.strokeWidth (Length.millimeters 1.0)
        ]
  let curveResolution = Resolution.maxError (Length.millimeters 0.1)
  let curveEntity = Drawing2d.curveWith curveAttributes curveResolution curve
  let coordinateBounds = Bounds (Length.centimeters -1.0) (Length.centimeters 11.0)
  let drawingBounds = Bounds2d coordinateBounds coordinateBounds
  Drawing2d.writeSvg "executables/sandbox/test-hermite-bezier.svg" drawingBounds curveEntity

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
  IO.printLine (xs <> ys)

textSum :: Text -> Text -> Result Text Int
textSum t1 t2 = Try.do
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
  let bounds = Bounds2d (Bounds 0.0 2.0) (Bounds 0.0 2.0)
  let function = VectorSurfaceFunction2d.xy f g
  let solution =
        Solve2d.unique
          (VectorSurfaceFunction2d.evaluateBounds function)
          (VectorSurfaceFunction2d.evaluate function)
          (VectorSurfaceFunction2d.evaluate (VectorSurfaceFunction2d.derivative SurfaceParameter.U function))
          (VectorSurfaceFunction2d.evaluate (VectorSurfaceFunction2d.derivative SurfaceParameter.V function))
          bounds
  log "Solve2d.unique solution" solution

testExpression :: IO ()
testExpression = IO.do
  let x = Expression.t
  let xSquared = Expression.squared x
  let expression = xSquared / (xSquared + Expression.Curve1d.constant 1.0)
  log "Expression value" (Expression.evaluate expression 2.0)
  log "Expression bounds" (Expression.evaluateBounds expression (Bounds 1.0 3.0))

testCurve2dExpression :: IO ()
testCurve2dExpression = IO.do
  let x = Expression.Curve1d.constant 10.0 * Expression.t
  let y = Expression.sqrt Expression.t
  let curve = Expression.xy x y :: Expression Float (Point2d (Global @ Unitless))
  log "Evaluated 2D curve" (Expression.evaluate curve 3.0)

main :: IO ()
main = Tolerance.using (Length.meters 1e-9) IO.do
  testScalarArithmetic
  testVectorArithmetic
  testBoundsArithmetic
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
  testExpression
  testCurve2dExpression
  testPlaneParaboloidIntersection
  testPlaneTorusIntersection
  testExplicitRandomStep
  testConcurrency
  testIOParallel
  testDebugPrint
  testTextSum
  testNewtonRaphson2d
