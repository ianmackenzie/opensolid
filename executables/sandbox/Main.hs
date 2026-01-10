-- {-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-local-binds -Wno-unused-matches -Wno-unused-top-binds #-}

module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Area qualified as Area
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Circle2d qualified as Circle2d
import OpenSolid.Color (Color)
import OpenSolid.Color qualified as Color
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Debug.Plot qualified as Debug.Plot
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Duration (Duration)
import OpenSolid.Duration qualified as Duration
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Curve1d qualified as Expression.Curve1d
import OpenSolid.IO qualified as IO
import OpenSolid.IO.Parallel qualified as IO.Parallel
import OpenSolid.Int qualified as Int
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Polygon2d (Polygon2d (Polygon2d))
import OpenSolid.Polyline2d (Polyline2d (Polyline2d))
import OpenSolid.Polyline2d qualified as Polyline2d
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Random qualified as Random
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Solve2d qualified as Solve2d
import OpenSolid.Surface3d qualified as Surface3d
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction.Zeros qualified as SurfaceFunction.Zeros
import OpenSolid.SurfaceParameter qualified as SurfaceParameter
import OpenSolid.Svg (Svg)
import OpenSolid.Svg qualified as Svg
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvBounds qualified as UvBounds
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d
import OpenSolid.Volume qualified as Volume
import OpenSolid.World3d qualified as World3d
import Prelude hiding (length, log, sum)

data Global deriving (Eq, Show)

log :: Show a => Text -> a -> IO ()
log label value = IO.printLine (label <> ": " <> Text.show value)

testScalarArithmetic :: IO ()
testScalarArithmetic = do
  log @Int "Integer product" (3 * 4)
  log @Int "Integer division" (10 `div` 4)
  log "True division" (Int.ratio 10 4)
  let area = Area.squareMeters 3
  let length = Length.centimeters 3
  let volume = area .*. length
  let volumeInCubicCentimeters = Volume.inCubicCentimeters volume
  log "Volume in cubic centimeters" volumeInCubicCentimeters
  log "sqrt 2" (Number.sqrt 2)

testVectorArithmetic :: IO ()
testVectorArithmetic = do
  let vector x y z = Vector3d.zUp (Length.meters x) (Length.meters y) (Length.meters z)
  let v1 = Vector2D.meters 1 2
  let v2 = 0.5 *. Vector2D.meters 3 4
  let dotProduct = v1 `dot` v2
  log "Dot product" dotProduct
  log "2D cross product" (v1 `cross` v2)
  let squareRoot = Quantity.sqrt dotProduct
  log "Square root" squareRoot
  let translatedPoint = Point2D.meters 2 3 .+. Vector2D.meters 4 5
  log "Translated point" translatedPoint
  let vectorSum = Vector2D.meters 1 2 .+. Vector2D.meters 2 3
  log "Vector sum" vectorSum
  let crossProduct = vector 1 2 3 `cross` vector 4 5 6
  log "Cross product" crossProduct
  let scaledVector = Length.meters 2 .*. Vector2D.meters 3 4
  log "Scaled vector" scaledVector

testBoundsArithmetic :: IO ()
testBoundsArithmetic = do
  let boundsDifference =
        Bounds (Length.meters 2) (Length.meters 3) .-. Length.centimeters 50
  log "Bounds difference" boundsDifference
  let boundsProduct =
        Length.centimeters 20 .*. Bounds (Length.meters 2) (Length.meters 3)
  log "Bounds product" boundsProduct

testEquality :: IO ()
testEquality = Tolerance.using Length.centimeter do
  log "Equality test" (Length.meters 1 ~= Length.meters 1.005)

testTransformation :: IO ()
testTransformation = do
  let rotatedAxis = Axis2d.rotateAround (Point2D.meters 1 0) Angle.quarterTurn Axis2d.x
  log "Rotated axis" rotatedAxis
  let originalPoints = [Point2D.meters 1 0, Point2D.meters 2 0, Point2D.meters 3 0]
  let rotationFunction = Point2D.rotateAround Point2D.origin Angle.quarterTurn
  let rotatedPoints = List.map rotationFunction originalPoints
  log "Rotated points" rotatedPoints

offsetPoint ::
  Tolerance Meters =>
  Point2D Meters space ->
  Point2D Meters space ->
  Length ->
  Point2D Meters space
offsetPoint startPoint endPoint distance =
  case Direction2d.from startPoint endPoint of
    Error Direction2d.PointsAreCoincident -> startPoint
    Ok direction -> do
      let displacement = distance .*. Direction2d.perpendicularTo direction
      Point2D.midpoint startPoint endPoint .+. displacement

testCustomFunction :: Tolerance Meters => IO ()
testCustomFunction = do
  let point = offsetPoint (Point2D.meters 1 0) (Point2D.meters 3 0) (Length.meters 1)
  log "Offset point" point

testListOperations :: IO ()
testListOperations = do
  let deltas :: [Int] = List.successive subtract [0, 1, 4, 9, 16, 25]
  let intervals :: [Bounds Unitless] = List.successive Bounds [1, 2, 3, 4]
  log "Successive deltas" deltas
  log "Successive intervals" intervals

getCrossProduct :: Tolerance Meters => Result Text Number
getCrossProduct = do
  vectorDirection <- Result.orFail (Vector2D.direction (Vector2D.meters 2 3))
  lineDirection <- Result.orFail (Direction2d.from Point2D.origin Point2D.origin)
  Ok (vectorDirection `cross` lineDirection)

testTry :: Tolerance Meters => IO ()
testTry =
  IO.onError IO.printLine do
    crossProduct <- Result.orFail getCrossProduct
    log "Got cross product" crossProduct

testIOIteration :: IO ()
testIOIteration = IO.forEach [1 :: Int .. 3] (log "Looping")

doublingIO :: Text -> IO Int
doublingIO input = do
  value <- Result.orFail (Int.parse input)
  let doubled = 2 * value
  return doubled

doubleManyIO :: IO [Int]
doubleManyIO = IO.collect doublingIO ["1", "-2", "3"]

testIOSequencing :: IO ()
testIOSequencing = do
  doubledValues <- doubleManyIO
  IO.forEach doubledValues (log "Doubled value")

testParameter1dGeneration :: IO ()
testParameter1dGeneration = do
  t1 <- Random.generate Parameter.random
  t2 <- Random.generate Parameter.random
  t3 <- Random.generate Parameter.random
  log "Random parameter value 1" t1
  log "Random parameter value 2" t2
  log "Random parameter value 3" t3

testEmptyCheck :: [Int] -> IO ()
testEmptyCheck [] = IO.printLine "List is empty"
testEmptyCheck (first : rest) =
  IO.printLine ("List is non-empty, maximum is " <> Text.int (NonEmpty.maximum (first :| rest)))

testNonEmpty :: IO ()
testNonEmpty = do
  testEmptyCheck []
  testEmptyCheck [2, 3, 1]

testPlaneTorusIntersection :: Tolerance Meters => IO ()
testPlaneTorusIntersection = do
  let minorRadius = Length.centimeters 1
  let majorRadius = Length.centimeters 2
  let crossSection = Curve2d.circle (Circle2d.withRadius minorRadius (Point2D.x majorRadius))
  surface <- Result.orFail (Surface3d.revolved World3d.frontPlane crossSection Axis2d.y Angle.twoPi)
  let alpha = Angle.asin (minorRadius ./. majorRadius)
  -- Other possibilities: Direction3d.xy (Angle.degrees 45), Direction3d.z
  let planeNormal = Direction3d.polar World3d.frontPlane (alpha .+. Angle.halfPi)
  let f = planeNormal `dot` (surface.function .-. World3d.originPoint)
  zeros <- Result.orFail (SurfaceFunction.zeros f)
  drawZeros "executables/sandbox/test-plane-torus-intersection.svg" zeros
  IO.printLine "Plane torus intersection solutions:"
  log "  Crossing curves" (List.length zeros.crossingCurves)
  log "  Saddle points" (List.length zeros.saddlePoints)

testPlaneParaboloidIntersection :: IO ()
testPlaneParaboloidIntersection = Tolerance.using 1e-9 do
  let u = SurfaceFunction.u
  let v = SurfaceFunction.v
  let f = SurfaceFunction.squared u .+. SurfaceFunction.squared v .- 0.5
  zeros <- Result.orFail (SurfaceFunction.zeros f)
  drawZeros "executables/sandbox/test-plane-paraboloid-intersection.svg" zeros
  IO.printLine "Plane paraboloid intersection solutions:"
  log "  Crossing curves" (List.length zeros.crossingCurves)
  log "  Saddle points" (List.length zeros.saddlePoints)

strokeWidth :: Length
strokeWidth = Length.millimeters 0.1

drawZeros :: Text -> SurfaceFunction.Zeros -> IO ()
drawZeros path zeros = do
  let uvBounds = Bounds.convert toDrawing (Bounds -0.05 1.05)
  let viewBox = Bounds2d uvBounds uvBounds
  Svg.write path viewBox $
    Svg.groupWith [Svg.strokeWidth strokeWidth] $
      [ drawBounds (Bounds2d.convert toDrawing UvBounds.unitSquare)
      , Svg.group (List.mapWithIndex drawCrossingCurve zeros.crossingCurves)
      , Svg.combine (drawDot Color.orange) zeros.saddlePoints
      ]

drawBounds :: Bounds2d Meters space -> Svg space
drawBounds bounds = do
  let corner x y = Bounds2d.interpolate bounds x y
  let vertices = NonEmpty.four (corner 0 0) (corner 1 0) (corner 1 1) (corner 0 1)
  Svg.polygon (Polygon2d vertices)

drawCrossingCurve :: Int -> Curve2d Unitless UvSpace -> Svg UvSpace
drawCrossingCurve index curve = do
  let hue = (Number.fromInt index .*. Angle.goldenAngle) .%. Angle.twoPi
  let color = Color.hsl1 hue 0.5 0.5
  drawUvCurve [Svg.strokeColor color] curve

toDrawing :: Quantity (Meters ?/? Unitless)
toDrawing = Length.centimeters 10 ?/ 1

drawUvCurve :: [Svg.Attribute UvSpace] -> Curve2d Unitless UvSpace -> Svg UvSpace
drawUvCurve attributes curve = do
  let resolution = Resolution.maxError 0.0002
  let polyline = Curve2d.toPolyline resolution curve
  Svg.polylineWith attributes (Polyline2d.map (Point2D.convert toDrawing) polyline)

drawDot :: Color -> UvPoint -> Svg UvSpace
drawDot color point =
  Svg.circleWith
    [Svg.fillColor color]
    (#centerPoint (Point2D.convert toDrawing point))
    (#diameter (Length.millimeters 1))

delayedPrint :: Int -> Duration -> IO ()
delayedPrint n delay = do
  IO.sleep delay
  IO.printLine (Text.int n)

testConcurrency :: IO ()
testConcurrency = do
  IO.printLine "Starting concurrency test..."
  IO.printLine "0"
  IO.Parallel.run
    [ delayedPrint 5 (Duration.milliseconds 250)
    , delayedPrint 2 (Duration.milliseconds 100)
    , delayedPrint 3 (Duration.milliseconds 150)
    , delayedPrint 4 (Duration.milliseconds 200)
    , delayedPrint 1 (Duration.milliseconds 50)
    ]
  IO.printLine "Concurrency test complete!"

computeSquareRoot :: Number -> IO Number
computeSquareRoot value = do
  IO.sleep (Duration.milliseconds 100)
  return (sqrt value)

testIOParallel :: IO ()
testIOParallel = do
  squareRoots <- IO.Parallel.collect computeSquareRoot [1, 4, 9, 16, 25]
  log "Square roots" squareRoots

drawBezier ::
  Tolerance Meters =>
  Color ->
  Point2D Unitless space ->
  [Point2D Unitless space] ->
  Point2D Unitless space ->
  Svg space
drawBezier color startPoint innerControlPoints endPoint = do
  let drawingStartPoint = Point2D.convert toDrawing startPoint
  let drawingEndPoint = Point2D.convert toDrawing endPoint
  let drawingInnerControlPoints = List.map (Point2D.convert toDrawing) innerControlPoints
  let drawingControlPoints = drawingStartPoint :| (drawingInnerControlPoints <> [drawingEndPoint])
  let curve = Curve2d.bezier drawingControlPoints
  let drawSegmentBounds tBounds = drawBounds (Curve2d.evaluateBounds curve tBounds)
  let controlPointDiameter = Length.millimeters 10
  let drawControlPoint point =
        Svg.circle (#centerPoint point) (#diameter controlPointDiameter)
  let resolution = Resolution.maxError Length.millimeter
  Svg.groupWith
    [ Svg.strokeColor color
    , Svg.strokeWidth (Length.millimeters 1)
    ]
    [ Svg.groupWith [Svg.opacity 0.3] $
        [ Svg.polyline (Polyline2d drawingControlPoints)
        , Svg.combineWith [Svg.fillColor color] drawControlPoint $
            NonEmpty.toList drawingControlPoints
        , Svg.combine drawSegmentBounds (Parameter.intervals 10)
        ]
    , Svg.curve resolution curve
    ]

testBezierSegment :: Tolerance Meters => IO ()
testBezierSegment = do
  let p1 = Point2D.origin @Unitless @Global
  let p2 = Point2D 0 5
  let p3 = Point2D 2.5 10
  let p4 = Point2D 5 0
  let p5 = Point2D 10 5
  let p6 = Point2D 10 10
  let coordinateBounds = Bounds.convert toDrawing (Bounds -1 11)
  let drawingBounds = Bounds2d coordinateBounds coordinateBounds
  let curveEntity = drawBezier Color.blue p1 [p2, p3, p4, p5] p6
  Svg.write "executables/sandbox/test-bezier-segment.svg" drawingBounds curveEntity

testHermiteBezier :: IO ()
testHermiteBezier = do
  let startPoint = Point2D.origin @Meters @Global
  let startDerivatives = [Vector2D.centimeters 10 10]
  let endDerivatives = [Vector2D.centimeters 0 -10, Vector2D.zero]
  let endPoint = Point2D.centimeters 10 0
  let curve = Curve2d.hermite startPoint startDerivatives endPoint endDerivatives
  let curveAttributes =
        [ Svg.strokeColor Color.blue
        , Svg.strokeWidth (Length.millimeters 1)
        ]
  let curveResolution = Resolution.maxError (Length.millimeters 0.1)
  let curveEntity = Svg.curveWith curveAttributes curveResolution curve
  let coordinateBounds = Bounds (Length.centimeters -1) (Length.centimeters 11)
  let drawingBounds = Bounds2d coordinateBounds coordinateBounds
  Svg.write "executables/sandbox/test-hermite-bezier.svg" drawingBounds curveEntity

testExplicitRandomStep :: IO ()
testExplicitRandomStep = do
  let seed0 = Random.init 1234
  let list0 :: [Int] = [5 .. 10]
  let generator = List.shuffle list0
  let (list1, seed1) = Random.step generator seed0
  log "list1" list1
  let (list2, seed2) = Random.step generator seed1
  log "list2" list2
  let (list3, _) = Random.step generator seed2
  log "list3" list3

textSum :: Text -> Text -> Result Text Int
textSum t1 t2 = do
  n1 <- Result.orFail (Int.parse t1)
  n2 <- Result.orFail (Int.parse t2)
  Ok (n1 + n2)

testTextSum :: IO ()
testTextSum = do
  IO.onError IO.printLine do
    sum <- Result.orFail (textSum "5" "abc")
    log "sum" sum
  IO.onError IO.printLine do
    sum <- Result.orFail (textSum "2" "3")
    log "sum" sum

testNewtonRaphson2d :: IO ()
testNewtonRaphson2d = Tolerance.using 1e-9 do
  let u = SurfaceFunction.u
  let v = SurfaceFunction.v
  let f = SurfaceFunction.squared u .+. SurfaceFunction.squared v .- 4
  let g = u .-. v
  let bounds = Bounds2d (Bounds 0 2) (Bounds 0 2)
  let function = VectorSurfaceFunction2d.xy f g
  let uDerivative = VectorSurfaceFunction2d.derivative SurfaceParameter.U function
  let vDerivative = VectorSurfaceFunction2d.derivative SurfaceParameter.V function
  let solution =
        Solve2d.unique
          (VectorSurfaceFunction2d.evaluateBounds function)
          (VectorSurfaceFunction2d.evaluate function)
          (VectorSurfaceFunction2d.evaluate uDerivative)
          (VectorSurfaceFunction2d.evaluate vDerivative)
          bounds
  log "Solve2d.unique solution" solution

testExpression :: IO ()
testExpression = do
  let x = Expression.t
  let xSquared = Expression.squared x
  let expression = xSquared ./. (xSquared .+. Expression.Curve1d.constant 1)
  log "Expression value" (Expression.evaluate expression 2)
  log "Expression bounds" (Expression.evaluateBounds expression (Bounds 1 3))

testCurve2dExpression :: IO ()
testCurve2dExpression = do
  let x = Expression.Curve1d.constant 10 .*. Expression.t
  let y = Expression.sqrt Expression.t
  let curve = Expression.xy x y :: Expression Number (Point2D Unitless Global)
  log "Evaluated 2D curve" (Expression.evaluate curve 3)

testQuotientDesingularization :: IO ()
testQuotientDesingularization = Tolerance.using 1e-9 do
  let numerator = Curve.sin (Angle.pi .*. Curve.t)
  let denominator = Curve.t .*. (1 -. Curve.t)
  quotient <- Result.orFail (Curve.quotient numerator denominator)
  let quotient' = Curve.derivative quotient
  let quotient'' = Curve.derivative quotient'
  let quotient''' = Curve.derivative quotient''
  let tValues = Quantity.steps 0 1 10
  IO.forEach tValues \tValue -> do
    log "quotient" (Curve.evaluate quotient tValue)
  IO.forEach tValues \tValue -> do
    log "derivative" (Curve.evaluate quotient' tValue)
  IO.forEach tValues \tValue -> do
    log "second derivative" (Curve.evaluate quotient'' tValue)
  IO.forEach tValues \tValue -> do
    log "third derivative" (Curve.evaluate quotient''' tValue)

testCurveSqrt :: IO ()
testCurveSqrt = Tolerance.using 1e-6 do
  let t = Curve.t
  let curve = Curve.sqrt (0.5 *. (1 -. Curve.cos (Angle.twoPi .*. t)))
  let curve' = Curve.derivative curve
  Svg.write
    "executables/sandbox/cos-sqrt.svg"
    (Debug.Plot.viewBox (Point2D -0.1 -4.1) (Point2D 1.1 4.1))
    ( Svg.group
        [ Debug.Plot.xAxis 0 1
        , Debug.Plot.yAxis 0 1
        , Debug.Plot.curve curve
        , Debug.Plot.curve curve'
        ]
    )

main :: IO ()
main = Tolerance.using Length.nanometer do
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
  testTextSum
  testNewtonRaphson2d
  testQuotientDesingularization
  testCurveSqrt
