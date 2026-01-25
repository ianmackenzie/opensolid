module OpenSolid.Curve2D
  ( Curve2D
  , Compiled
  , new
  , recursive
  , constant
  , xy
  , line
  , lineFrom
  , arcFrom
  , polarArc
  , sweptArc
  , cornerArc
  , WhichArc (..)
  , radiusArc
  , ellipticalArc
  , customArc
  , circle
  , ellipse
  , bezier
  , quadraticBezier
  , cubicBezier
  , hermite
  , desingularize
  , desingularized
  , evaluate
  , evaluateAt
  , startPoint
  , endPoint
  , endpoints
  , evaluateBounds
  , compiled
  , derivative
  , secondDerivative
  , tangentDirection
  , offsetLeftwardBy
  , offsetRightwardBy
  , reverse
  , bounds
  , g2
  , Intersections (IntersectionPoints, OverlappingSegments)
  , IntersectionPoint
  , OverlappingSegment (OverlappingSegment)
  , intersections
  , findPoint
  , distanceAlong
  , distanceLeftOf
  , distanceRightOf
  , isPoint
  , isOnAxis
  , xCoordinate
  , yCoordinate
  , placeIn
  , relativeTo
  , placeOn
  , transformBy
  , translateBy
  , translateIn
  , translateAlong
  , rotateAround
  , mirrorAcross
  , scaleAbout
  , scaleAlong
  , convert
  , unconvert
  , curvature
  , curvature_
  , toPolyline
  , samplingPoints
  , medialAxis
  , arcLengthParameterization
  , parameterizeByArcLength
  , piecewise
  )
where

import GHC.Records (HasField (getField))
import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.ArcLength qualified as ArcLength
import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.Axis2D (Axis2D (Axis2D))
import OpenSolid.Bezier qualified as Bezier
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Circle2D (Circle2D)
import OpenSolid.Circle2D qualified as Circle2D
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import OpenSolid.Curve (IsPoint (IsPoint))
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve2D.IntersectionPoint (IntersectionPoint)
import {-# SOURCE #-} OpenSolid.Curve2D.Intersections
  ( Intersections (IntersectionPoints, OverlappingSegments)
  , intersections
  )
import OpenSolid.Curve2D.MedialAxis qualified as MedialAxis
import OpenSolid.Curve2D.OverlappingSegment (OverlappingSegment (OverlappingSegment))
import {-# SOURCE #-} OpenSolid.Curve3D (Curve3D)
import {-# SOURCE #-} OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.CurveParameter (CurveParameter (T))
import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.Differentiable (Differentiable)
import OpenSolid.Differentiable qualified as Differentiable
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.DirectionCurve2D (DirectionCurve2D)
import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Domain1D qualified as Domain1D
import OpenSolid.Expression qualified as Expression
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Line2D (Line2D (Line2D))
import OpenSolid.Linearization qualified as Linearization
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Orientation2D (Orientation2D)
import OpenSolid.Orientation2D qualified as Orientation2D
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Polyline2D (Polyline2D (Polyline2D))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Resolution (Resolution)
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.SurfaceFunction1D.Zeros qualified as SurfaceFunction1D.Zeros
import {-# SOURCE #-} OpenSolid.SurfaceFunction2D (SurfaceFunction2D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction2D qualified as SurfaceFunction2D
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform2D (Transform2D)
import OpenSolid.Transform2D qualified as Transform2D
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector qualified as Vector
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.VectorBounds2D (VectorBounds2D)
import OpenSolid.VectorBounds2D qualified as VectorBounds2D
import OpenSolid.VectorCurve2D (VectorCurve2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D
import OpenSolid.VectorCurve3D (VectorCurve3D)
import OpenSolid.VectorCurve3D qualified as VectorCurve3D
import OpenSolid.VectorSurfaceFunction2D qualified as VectorSurfaceFunction2D
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)

-- | A parametric curve in 2D space.
data Curve2D units space = Curve2D
  { compiled :: Compiled units space
  , derivative :: ~(VectorCurve2D units space)
  }

type Compiled units space =
  CompiledFunction
    Number
    (Point2D units space)
    (Interval Unitless)
    (Bounds2D units space)

instance HasField "xCoordinate" (Curve2D units space) (Curve1D units) where
  getField = xCoordinate

instance HasField "yCoordinate" (Curve2D units space) (Curve1D units) where
  getField = yCoordinate

instance HasField "coordinates" (Curve2D units space) (Curve1D units, Curve1D units) where
  getField = coordinates

instance FFI (Curve2D Meters FFI.Space) where
  representation = FFI.classRepresentation "Curve2D"

instance FFI (Curve2D Unitless UvSpace) where
  representation = FFI.classRepresentation "UvCurve"

instance HasUnits (Curve2D units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (Curve2D unitsA space1) (Curve2D unitsB space2)
  where
  coerce curve = Curve2D (Units.coerce curve.compiled) (Units.coerce curve.derivative)

instance Differentiable CurveParameter (Curve2D units space) (VectorCurve2D units space) where
  derivative T = derivative

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Curve2D units1 space1) (Point2D units2 space2) units1
  where
  curve `intersects` point = (curve .-. point) `intersects` Vector2D.zero

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Point2D units1 space1) (Curve2D units2 space2) units1
  where
  point `intersects` curve = curve `intersects` point

instance ApproximateEquality (Curve2D units space) units where
  curve1 ~= curve2 = samplePoints curve1 ~= samplePoints curve2

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Curve2D units1 space1)
    (VectorCurve2D units2 space2)
    (Curve2D units1 space1)
  where
  lhs .+. rhs =
    new
      (compiled lhs .+. VectorCurve2D.compiled rhs)
      (derivative lhs .+. VectorCurve2D.derivative rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2D units1 space1)
    (VectorCurve2D units2 space2)
    (Curve2D units1 space1)
  where
  lhs .-. rhs =
    new
      (compiled lhs .-. VectorCurve2D.compiled rhs)
      (derivative lhs .-. VectorCurve2D.derivative rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Curve2D units1 space1)
    (Vector2D units2 space2)
    (Curve2D units1 space1)
  where
  lhs .+. rhs = lhs .+. VectorCurve2D.constant rhs

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2D units1 space1)
    (Vector2D units2 space2)
    (Curve2D units1 space1)
  where
  lhs .-. rhs = lhs .-. VectorCurve2D.constant rhs

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2D units1 space1)
    (Curve2D units2 space2)
    (VectorCurve2D units1 space1)
  where
  lhs .-. rhs =
    VectorCurve2D.new (lhs.compiled .-. rhs.compiled) (lhs.derivative .-. rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2D units1 space1)
    (Point2D units2 space2)
    (VectorCurve2D units1 space1)
  where
  curve .-. point = curve .-. constant point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2D units1 space1)
    (Curve2D units2 space2)
    (VectorCurve2D units1 space1)
  where
  point .-. curve = constant point .-. curve

instance Composition (Curve1D Unitless) (Curve2D units space) (Curve2D units space) where
  f `compose` g =
    new (f.compiled `compose` g.compiled) ((f.derivative `compose` g) .*. g.derivative)

instance
  Composition
    (SurfaceFunction1D Unitless)
    (Curve2D units space)
    (SurfaceFunction2D units space)
  where
  curve `compose` function =
    SurfaceFunction2D.new
      (curve.compiled `compose` function.compiled)
      (\p -> curve.derivative `compose` function .*. SurfaceFunction1D.derivative p function)

instance
  Composition
    SurfaceParameter
    (Curve2D units space)
    (SurfaceFunction2D units space)
  where
  curve `compose` parameter = curve `compose` SurfaceFunction1D.parameter parameter

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (Curve2D unitless uvSpace)
    (SurfaceFunction1D units)
    (Curve1D units)
  where
  f `compose` g = do
    let (dudt, dvdt) = VectorCurve2D.components g.derivative
    Curve1D.new
      (f.compiled `compose` g.compiled)
      (f.du `compose` g .*. dudt .+. f.dv `compose` g .*. dvdt)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (Curve2D unitless uvSpace)
    (VectorSurfaceFunction3D units space)
    (VectorCurve3D units space)
  where
  function `compose` uvCurve = do
    let (dudt, dvdt) = VectorCurve2D.components uvCurve.derivative
    VectorCurve3D.new
      (function.compiled `compose` uvCurve.compiled)
      (function.du `compose` uvCurve .*. dudt .+. function.dv `compose` uvCurve .*. dvdt)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (Curve2D unitless uvSpace)
    (SurfaceFunction3D space)
    (Curve3D space)
  where
  function `compose` uvCurve = do
    let (dudt, dvdt) = VectorCurve2D.components uvCurve.derivative
    Curve3D.new
      (function.compiled `compose` uvCurve.compiled)
      (function.du `compose` uvCurve .*. dudt .+. function.dv `compose` uvCurve .*. dvdt)

new :: Compiled units space -> VectorCurve2D units space -> Curve2D units space
new = Curve2D

recursive ::
  Compiled units space ->
  (Curve2D units space -> VectorCurve2D units space) ->
  Curve2D units space
recursive givenCompiled derivativeFunction =
  let result = new givenCompiled (derivativeFunction result) in result

-- | Create a degenerate curve that is actually just a single point.
constant :: Point2D units space -> Curve2D units space
constant point = new (CompiledFunction.constant point) VectorCurve2D.zero

-- | Create a curve from its X and Y coordinate curves.
xy :: Curve1D units -> Curve1D units -> Curve2D units space
xy x y = do
  let compiledXY = CompiledFunction.map2 Expression.xy Point2D Bounds2D x.compiled y.compiled
  new compiledXY (VectorCurve2D.xy x.derivative y.derivative)

-- | Convert a line to a curve.
line :: Line2D units space -> Curve2D units space
line (Line2D p1 p2) = lineFrom p1 p2

-- | Create a line between two points.
lineFrom :: Point2D units space -> Point2D units space -> Curve2D units space
lineFrom p1 p2 = bezier (NonEmpty.two p1 p2)

{-| Create an arc from the given start point to the given end point, with the given swept angle.

A positive swept angle means the arc turns counterclockwise (turns to the left),
and a negative swept angle means it turns clockwise (turns to the right).
For example, an arc with a swept angle of positive 90 degrees
is quarter circle that turns to the left.
-}
arcFrom ::
  Tolerance units =>
  Point2D units space ->
  Point2D units space ->
  Angle ->
  Curve2D units space
arcFrom givenStartPoint givenEndPoint sweptAngle =
  case Vector2D.magnitudeAndDirection (givenEndPoint .-. givenStartPoint) of
    Error Vector.IsZero -> lineFrom givenStartPoint givenEndPoint
    Ok (distanceBetweenPoints, directionBetweenPoints) -> do
      let halfDistance = 0.5 *. distanceBetweenPoints
      let tanHalfAngle = Angle.tan (0.5 *. sweptAngle)
      let linearDeviation = halfDistance .*. tanHalfAngle
      if linearDeviation ~= Quantity.zero
        then lineFrom givenStartPoint givenEndPoint
        else do
          let offset = (halfDistance ./. tanHalfAngle) .*. Direction2D.rotateLeft directionBetweenPoints
          let centerPoint = Point2D.midpoint givenStartPoint givenEndPoint .+. offset
          let radius = Point2D.distanceFrom centerPoint givenStartPoint
          let xVector = Vector2D.x radius
          let yVector = Vector2D.y radius
          let startAngle = Point2D.angleFrom centerPoint givenStartPoint
          let endAngle = startAngle .+. sweptAngle
          customArc centerPoint xVector yVector startAngle endAngle

-- | Create an arc with the given center point, radius, start angle and end angle.
polarArc ::
  "centerPoint" ::: Point2D units space ->
  "radius" ::: Quantity units ->
  "startAngle" ::: Angle ->
  "endAngle" ::: Angle ->
  Curve2D units space
polarArc (Named centerPoint) (Named radius) (Named startAngle) (Named endAngle) =
  customArc centerPoint (Vector2D.x radius) (Vector2D.y radius) startAngle endAngle

{-| Create an arc with the given center point, start point and swept angle.

The start point will be swept around the center point by the given angle.
-}
sweptArc :: Point2D units space -> Point2D units space -> Angle -> Curve2D units space
sweptArc centerPoint givenStartPoint sweptAngle = do
  let radius = Point2D.distanceFrom centerPoint givenStartPoint
  let startAngle = Point2D.angleFrom centerPoint givenStartPoint
  polarArc
    (#centerPoint centerPoint)
    (#radius radius)
    (#startAngle startAngle)
    (#endAngle (startAngle .+. sweptAngle))

-- | Create an arc for rounding off the corner between two straight lines.
cornerArc ::
  Tolerance units =>
  Point2D units space ->
  "incoming" ::: Direction2D space ->
  "outgoing" ::: Direction2D space ->
  "radius" ::: Quantity units ->
  Curve2D units space
cornerArc cornerPoint (Named incomingDirection) (Named outgoingDirection) (Named givenRadius) = do
  let radius = Quantity.abs givenRadius
  let sweptAngle = Direction2D.angleFrom incomingDirection outgoingDirection
  if 0.25 *. radius .*. Number.squared (Angle.inRadians sweptAngle) ~= Quantity.zero
    then lineFrom cornerPoint cornerPoint
    else do
      let offset = radius .*. Number.abs (Angle.tan (0.5 *. sweptAngle))
      let computedStartPoint = cornerPoint .-. offset .*. incomingDirection
      let computedEndPoint = cornerPoint .+. offset .*. outgoingDirection
      arcFrom computedStartPoint computedEndPoint sweptAngle

data WhichArc
  = SmallCounterclockwise
  | SmallClockwise
  | LargeCounterclockwise
  | LargeClockwise

radiusArc ::
  Tolerance units =>
  Quantity units ->
  Point2D units space ->
  Point2D units space ->
  WhichArc ->
  Curve2D units space
radiusArc givenRadius givenStartPoint givenEndPoint whichArc =
  case Direction2D.from givenStartPoint givenEndPoint of
    Ok chordDirection -> do
      let halfDistance = 0.5 *. Point2D.distanceFrom givenStartPoint givenEndPoint
      let radius = max (Quantity.abs givenRadius) halfDistance
      let offsetMagnitude =
            Quantity.sqrt_ (Quantity.squared_ radius .-. Quantity.squared_ halfDistance)
      let offsetDirection = Direction2D.rotateLeft chordDirection
      let offsetDistance =
            case whichArc of
              SmallCounterclockwise -> offsetMagnitude
              SmallClockwise -> negative offsetMagnitude
              LargeClockwise -> offsetMagnitude
              LargeCounterclockwise -> negative offsetMagnitude
      let offset = offsetDirection .*. offsetDistance
      let centerPoint = Point2D.midpoint givenStartPoint givenEndPoint .+. offset
      let shortAngle = 2 *. Angle.asin (halfDistance ./. givenRadius)
      let sweptAngle =
            case whichArc of
              SmallCounterclockwise -> shortAngle
              SmallClockwise -> negative shortAngle
              LargeClockwise -> shortAngle .-. Angle.twoPi
              LargeCounterclockwise -> Angle.twoPi .-. shortAngle
      sweptArc centerPoint givenStartPoint sweptAngle
    Error Direction2D.PointsAreCoincident ->
      lineFrom givenStartPoint givenEndPoint

ellipticalArc ::
  Frame2D units global local ->
  Quantity units ->
  Quantity units ->
  Angle ->
  Angle ->
  Curve2D units global
ellipticalArc axes xRadius yRadius startAngle endAngle = do
  let centerPoint = Frame2D.originPoint axes
  let xVector = xRadius .*. Frame2D.xDirection axes
  let yVector = yRadius .*. Frame2D.yDirection axes
  customArc centerPoint xVector yVector startAngle endAngle

customArc ::
  Point2D units space ->
  Vector2D units space ->
  Vector2D units space ->
  Angle ->
  Angle ->
  Curve2D units space
customArc p0 v1 v2 a b = do
  let angle = Curve1D.interpolateFrom a b
  p0 .+. v1 .*. Curve1D.cos angle .+. v2 .*. Curve1D.sin angle

-- | Create a curve from the given circle.
circle :: Circle2D units space -> Curve2D units space
circle givenCircle =
  polarArc
    (#centerPoint (Circle2D.centerPoint givenCircle))
    (#radius (Circle2D.radius givenCircle))
    (#startAngle Angle.zero)
    (#endAngle Angle.twoPi)

{-| Create an ellipes with the given principal axes and major/minor radii.
The first radius given will be the radius along the X axis,
and the second radius will be the radius along the Y axis.
-}
ellipse ::
  Frame2D units global local ->
  Quantity units ->
  Quantity units ->
  Curve2D units global
ellipse axes xRadius yRadius = ellipticalArc axes xRadius yRadius Angle.zero Angle.twoPi

{-| Construct a Bezier curve from its control points.

For example,

> Curve2D.bezier (NonEmpty.four p1 p2 p3 p4))

will return a cubic Bezier curve with the given four control points.
-}
bezier :: NonEmpty (Point2D units space) -> Curve2D units space
bezier controlPoints =
  new
    (CompiledFunction.concrete (Expression.bezierCurve controlPoints))
    (VectorCurve2D.bezier (Bezier.derivative controlPoints))

-- | Construct a quadratic Bezier curve from the given control points.
quadraticBezier ::
  Point2D units space ->
  Point2D units space ->
  Point2D units space ->
  Curve2D units space
quadraticBezier p1 p2 p3 = bezier (NonEmpty.three p1 p2 p3)

-- | Construct a cubic Bezier curve from the given control points.
cubicBezier ::
  Point2D units space ->
  Point2D units space ->
  Point2D units space ->
  Point2D units space ->
  Curve2D units space
cubicBezier p1 p2 p3 p4 = bezier (NonEmpty.four p1 p2 p3 p4)

{-| Construct a Bezier curve with the given endpoints and derivatives at those endpoints.

For example,

> Curve2D.hermite p1 [v1] p2 [v2]

will result in a cubic spline from @p1@ to @p2@ with first derivative equal to @v1@ at @p1@ and
first derivative equal to @v2@ at @p2@.

The numbers of derivatives at each endpoint do not have to be equal; for example,

> Curve2D.hermite p1 [v1] p2 []

will result in a quadratic spline from @p1@ to @p2@ with first derivative at @p1@ equal to @v1@.

In general, the degree of the resulting spline will be equal to 1 plus the total number of
derivatives given.
-}
hermite ::
  Point2D units space ->
  List (Vector2D units space) ->
  Point2D units space ->
  List (Vector2D units space) ->
  Curve2D units space
hermite start startDerivatives end endDerivatives =
  bezier (Bezier.hermite start startDerivatives end endDerivatives)

desingularize ::
  Maybe (Point2D units space, Vector2D units space) ->
  Curve2D units space ->
  Maybe (Point2D units space, Vector2D units space) ->
  Curve2D units space
desingularize Nothing curve Nothing = curve
desingularize startSingularity curve endSingularity = do
  let startCurve = case startSingularity of
        Nothing -> curve
        Just (value0, firstDerivative0) -> do
          let t0 = Desingularization.t0
          let valueT0 = evaluate curve t0
          let firstDerivativeT0 = VectorCurve2D.evaluate (derivative curve) t0
          let secondDerivativeT0 = VectorCurve2D.evaluate (secondDerivative curve) t0
          bezier $
            Bezier.syntheticStart
              value0
              firstDerivative0
              valueT0
              firstDerivativeT0
              secondDerivativeT0
  let endCurve = case endSingularity of
        Nothing -> curve
        Just (value1, firstDerivative1) -> do
          let t1 = Desingularization.t1
          let valueT1 = evaluate curve t1
          let firstDerivativeT1 = VectorCurve2D.evaluate (derivative curve) t1
          let secondDerivativeT1 = VectorCurve2D.evaluate (secondDerivative curve) t1
          bezier $
            Bezier.syntheticEnd
              valueT1
              firstDerivativeT1
              secondDerivativeT1
              value1
              firstDerivative1
  desingularized startCurve curve endCurve

desingularized ::
  Curve2D units space ->
  Curve2D units space ->
  Curve2D units space ->
  Curve2D units space
desingularized start middle end =
  new
    (CompiledFunction.desingularized Curve1D.t.compiled start.compiled middle.compiled end.compiled)
    (VectorCurve2D.desingularized start.derivative middle.derivative end.derivative)

instance HasField "startPoint" (Curve2D units space) (Point2D units space) where
  getField curve = evaluate curve 0

instance HasField "endPoint" (Curve2D units space) (Point2D units space) where
  getField curve = evaluate curve 1

{-| Evaluate a curve at a given parameter value.

The parameter value should be between 0 and 1.
-}
evaluate :: Curve2D units space -> Number -> Point2D units space
evaluate curve tValue = CompiledFunction.evaluate curve.compiled tValue

{-# INLINE evaluateAt #-}
evaluateAt :: Number -> Curve2D units space -> Point2D units space
evaluateAt tValue curve = evaluate curve tValue

-- | Get the start point of a curve.
startPoint :: Curve2D units space -> Point2D units space
startPoint curve = evaluate curve 0

-- | Get the end point of a curve.
endPoint :: Curve2D units space -> Point2D units space
endPoint curve = evaluate curve 1

-- | Get the start and end points of a curve.
endpoints :: Curve2D units space -> (Point2D units space, Point2D units space)
endpoints curve = (startPoint curve, endPoint curve)

evaluateBounds :: Curve2D units space -> Interval Unitless -> Bounds2D units space
evaluateBounds curve tBounds = CompiledFunction.evaluateBounds curve.compiled tBounds

samplePoints :: Curve2D units space -> NonEmpty (Point2D units space)
samplePoints curve = NonEmpty.map (evaluate curve) Parameter.samples

-- | Reverse a curve, so that the start point is the end point and vice versa.
reverse :: Curve2D units space -> Curve2D units space
reverse curve = curve `compose` (1 -. Curve1D.t)

bounds :: Curve2D units space -> Bounds2D units space
bounds curve = evaluateBounds curve Interval.unit

compiled :: Curve2D units space -> Compiled units space
compiled = (.compiled)

derivative :: Curve2D units space -> VectorCurve2D units space
derivative = (.derivative)

secondDerivative :: Curve2D units space -> VectorCurve2D units space
secondDerivative = Curve.secondDerivative

tangentDirection ::
  Tolerance units =>
  Curve2D units space ->
  Result IsPoint (DirectionCurve2D space)
tangentDirection = Curve.tangentDirection

offsetLeftwardBy ::
  Tolerance units =>
  Quantity units ->
  Curve2D units space ->
  Result IsPoint (Curve2D units space)
offsetLeftwardBy offset curve = do
  tangentCurve <- tangentDirection curve
  let offsetCurve = VectorCurve2D.rotateBy Angle.quarterTurn (offset .*. tangentCurve)
  Ok (curve .+. offsetCurve)

offsetRightwardBy ::
  Tolerance units =>
  Quantity units ->
  Curve2D units space ->
  Result IsPoint (Curve2D units space)
offsetRightwardBy distance = offsetLeftwardBy (negative distance)

distanceAlong :: Axis2D units space -> Curve2D units space -> Curve1D units
distanceAlong (Axis2D p0 d) curve = (curve .-. p0) `dot` d

distanceLeftOf :: Axis2D units space -> Curve2D units space -> Curve1D units
distanceLeftOf (Axis2D p0 d) curve = (curve .-. p0) `dot` Direction2D.rotateLeft d

distanceRightOf :: Axis2D units space -> Curve2D units space -> Curve1D units
distanceRightOf (Axis2D p0 d) curve = (curve .-. p0) `dot` Direction2D.rotateRight d

isPoint :: Tolerance units => Curve2D units space -> Bool
isPoint curve = VectorCurve2D.isZero curve.derivative

{-| Check if the given curve curve is collinear with (lies on) the given axis.

If the curve merely intersects/touches the axis at one or more points,
then it is not considered to lie on the axis;
it is only considered to lie on the axis if every point on the curve is also on the axis.
-}
isOnAxis :: Tolerance units => Axis2D units space -> Curve2D units space -> Bool
isOnAxis axis curve = NonEmpty.allSatisfy (intersects axis) (samplePoints curve)

-- | Get the X coordinate of a 2D curve as a scalar curve.
xCoordinate :: Curve2D units space -> Curve1D units
xCoordinate curve = do
  let compiledXCoordinate =
        CompiledFunction.map
          Expression.xCoordinate
          Point2D.xCoordinate
          Bounds2D.xCoordinate
          curve.compiled
  Curve1D.new compiledXCoordinate (VectorCurve2D.xComponent curve.derivative)

-- | Get the Y coordinate of a 2D curve as a scalar curve.
yCoordinate :: Curve2D units space -> Curve1D units
yCoordinate curve = do
  let compiledYCoordinate =
        CompiledFunction.map
          Expression.yCoordinate
          Point2D.yCoordinate
          Bounds2D.yCoordinate
          curve.compiled
  Curve1D.new compiledYCoordinate (VectorCurve2D.yComponent curve.derivative)

coordinates :: Curve2D units space -> (Curve1D units, Curve1D units)
coordinates curve = (xCoordinate curve, yCoordinate curve)

findPoint ::
  Tolerance units =>
  Point2D units space ->
  Curve2D units space ->
  Result IsPoint (List Number)
findPoint = Curve.findPoint

g2 ::
  Tolerance units =>
  (Curve2D units space, Number) ->
  (Curve2D units space, Number) ->
  Quantity units ->
  Bool
g2 (curve1, t1) (curve2, t2) radius =
  evaluate curve1 t1 ~= evaluate curve2 t2 && do
    let first1 = VectorCurve2D.evaluate curve1.derivative t1
    let first2 = VectorCurve2D.evaluate curve2.derivative t2
    let Vector2D dxdt1 dydt1 = first1
    let Vector2D dxdt2 dydt2 = first2
    let dxdtMin = min (Quantity.abs dxdt1) (Quantity.abs dxdt2)
    let dydtMin = min (Quantity.abs dydt1) (Quantity.abs dydt2)
    let orientation =
          if dxdtMin >= dydtMin
            then Orientation2D.horizontal
            else Orientation2D.vertical
    let signature1 = signature orientation curve1 t1 radius
    let signature2 = signature orientation curve2 t2 radius
    signature1 ~= signature2

signature ::
  Tolerance units =>
  Orientation2D space ->
  Curve2D units space ->
  Number ->
  Quantity units ->
  (Quantity units, Quantity units)
signature orientation curve tValue radius = do
  let local vector = Vector2D.relativeToOrientation orientation vector
  let curveFirstDerivative = derivative curve
  let curveSecondDerivative = secondDerivative curve
  let firstDerivativeValue = VectorCurve2D.evaluate curveFirstDerivative tValue
  let secondDerivativeValue = VectorCurve2D.evaluate curveSecondDerivative tValue
  let Vector2D x' y' = local firstDerivativeValue
  let Vector2D x'' y'' = local secondDerivativeValue
  let dydx = if x' != Quantity.zero then y' ./. x' else y'' ./. x''
  let firstOrder = dydx .*. radius
  let d2ydx2 =
        if x' != Quantity.zero
          then (y'' ?*? x' .-. y' ?*? x'') ?/? (x' ?*? x' ?*? x')
          else do
            let curveThirdDerivative = VectorCurve2D.derivative curveSecondDerivative
            let curveFourthDerivative = VectorCurve2D.derivative curveThirdDerivative
            let fourthDerivativeValue = VectorCurve2D.evaluate curveFourthDerivative tValue
            let Vector2D x'''' y'''' = local fourthDerivativeValue
            (y'''' ?*? x'' .-. y'' ?*? x'''') ?/? (x'' ?*? x'' ?*? x'')
  let secondOrder = Units.simplify (0.5 *. d2ydx2 ?*? Quantity.squared_ radius)
  (firstOrder, secondOrder)

placeIn ::
  Frame2D units global local ->
  Curve2D units local ->
  Curve2D units global
placeIn frame curve = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.placeIn frame)
          (Point2D.placeIn frame)
          (Bounds2D.placeIn frame)
          curve.compiled
  new compiledPlaced (VectorCurve2D.placeIn frame curve.derivative)

relativeTo ::
  Frame2D units global local ->
  Curve2D units global ->
  Curve2D units local
relativeTo frame = placeIn (Frame2D.inverse frame)

placeOn :: Plane3D global local -> Curve2D Meters local -> Curve3D global
placeOn plane curve = Curve3D.on plane curve

transformBy ::
  Transform2D tag units space ->
  Curve2D units space ->
  Curve2D units space
transformBy transform curve = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.transformBy transform)
          (Point2D.transformBy transform)
          (Bounds2D.transformBy transform)
          curve.compiled
  new compiledTransformed (VectorCurve2D.transformBy transform curve.derivative)

-- | Translate by the given displacement.
translateBy ::
  Vector2D units space ->
  Curve2D units space ->
  Curve2D units space
translateBy = Transform2D.translateByImpl transformBy

-- | Translate in the given direction by the given distance.
translateIn ::
  Direction2D space ->
  Quantity units ->
  Curve2D units space ->
  Curve2D units space
translateIn = Transform2D.translateInImpl transformBy

-- | Translate along the given axis by the given distance.
translateAlong ::
  Axis2D units space ->
  Quantity units ->
  Curve2D units space ->
  Curve2D units space
translateAlong = Transform2D.translateAlongImpl transformBy

-- | Rotate around the given point by the given angle.
rotateAround ::
  Point2D units space ->
  Angle ->
  Curve2D units space ->
  Curve2D units space
rotateAround = Transform2D.rotateAroundImpl transformBy

-- | Mirror across the given axis.
mirrorAcross ::
  Axis2D units space ->
  Curve2D units space ->
  Curve2D units space
mirrorAcross = Transform2D.mirrorAcrossImpl transformBy

-- | Scale uniformly about the given point by the given scaling factor.
scaleAbout ::
  Point2D units space ->
  Number ->
  Curve2D units space ->
  Curve2D units space
scaleAbout = Transform2D.scaleAboutImpl transformBy

-- | Scale (stretch) along the given axis by the given scaling factor.
scaleAlong ::
  Axis2D units space ->
  Number ->
  Curve2D units space ->
  Curve2D units space
scaleAlong = Transform2D.scaleAlongImpl transformBy

convert ::
  Quantity (units2 ?/? units1) ->
  Curve2D units1 space ->
  Curve2D units2 space
convert factor curve = Units.coerce (scaleAbout Point2D.origin (Units.erase factor) curve)

unconvert ::
  Quantity (units2 ?/? units1) ->
  Curve2D units2 space ->
  Curve2D units1 space
unconvert factor curve = convert (Units.simplify (1 /? factor)) curve

curvature_ ::
  Tolerance units =>
  Curve2D units space ->
  Result IsPoint (Curve1D (Unitless ?/? units))
curvature_ curve = do
  tangent <- tangentDirection curve
  let numerator = tangent `cross` secondDerivative curve
  let denominator = VectorCurve2D.squaredMagnitude_ (derivative curve)
  case Tolerance.using (Quantity.squared_ ?tolerance) (Curve1D.quotient_ numerator denominator) of
    Ok quotient_ -> Ok (Units.simplify quotient_)
    Error DivisionByZero -> Error IsPoint

{-| Get the curvature of a 2D curve.

This is the inverse of the radius of curvature, but is in general a better-defined quantity
since the radius of curvature can go to infinity if the curve has zero curvature anywhere,
and can in fact go through a singularity where it flips from positive to negative infinity
if the curve has an inflection point where curvature goes from positive to zero to negative.

Positive curvature is defined as curving to the left (relative to the curve's tangent direction).
-}
curvature ::
  (Tolerance units1, Units.Inverse units1 units2) =>
  Curve2D units1 space ->
  Result IsPoint (Curve1D units2)
curvature curve = Result.map Units.specialize (curvature_ curve)

toPolyline :: Resolution units -> Curve2D units space -> Polyline2D units space
toPolyline resolution curve =
  Polyline2D (NonEmpty.map (evaluate curve) (samplingPoints resolution curve))

samplingPoints :: Resolution units -> Curve2D units space -> NonEmpty Number
samplingPoints resolution curve = do
  let size subdomain = do
        let start = evaluate curve subdomain.lower
        let end = evaluate curve subdomain.upper
        Point2D.distanceFrom start end
  let curveSecondDerivative = secondDerivative curve
  let error subdomain = do
        let secondDerivativeBounds = VectorCurve2D.evaluateBounds curveSecondDerivative subdomain
        let secondDerivativeMagnitude = VectorBounds2D.magnitude secondDerivativeBounds
        Linearization.error secondDerivativeMagnitude subdomain
  let predicate = Resolution.predicate (#size size) (#error error) resolution
  Domain1D.samplingPoints predicate

medialAxis ::
  forall units space.
  Tolerance units =>
  Curve2D units space ->
  Curve2D units space ->
  Result IsPoint (List (MedialAxis.Segment units space))
medialAxis curve1 curve2 = do
  let p1 = curve1 `compose` SurfaceFunction1D.u
  let p2 = curve2 `compose` SurfaceFunction1D.v
  let v1 = curve1.derivative `compose` SurfaceFunction1D.u
  let v2 = curve2.derivative `compose` SurfaceFunction1D.v
  let d = p2 .-. p1
  let target =
        v2 `cross_` (2 *. (v1 `dot_` d) ?*? d .-. VectorSurfaceFunction2D.squaredMagnitude_ d ?*? v1)
  let targetTolerance = ?tolerance ?*? ((?tolerance ?*? ?tolerance) ?*? ?tolerance)
  case Tolerance.using targetTolerance (SurfaceFunction1D.zeros target) of
    Error SurfaceFunction1D.IsZero -> TODO -- curves are identical arcs?
    Ok zeros ->
      assert (List.isEmpty zeros.crossingLoops && List.isEmpty zeros.tangentPoints) do
        tangentDirection1 <- tangentDirection curve1
        let tangentVector1 = VectorCurve2D.unit tangentDirection1
        let normal1 = VectorCurve2D.rotateBy Angle.quarterTurn tangentVector1
        let radius :: SurfaceFunction1D units =
              Units.coerce $
                SurfaceFunction1D.unsafeQuotient_
                  (d `dot_` d)
                  (2 *. (tangentVector1 `compose` SurfaceFunction1D.u) `cross` d)
        let curve :: SurfaceFunction2D units space =
              (curve1 `compose` SurfaceFunction1D.u) .+. radius .*. (normal1 `compose` SurfaceFunction1D.u)
        let toSegment solutionCurve =
              MedialAxis.Segment
                { t1 = solutionCurve.xCoordinate
                , t2 = solutionCurve.yCoordinate
                , t12 = solutionCurve
                , curve = curve `compose` solutionCurve
                , radius = radius `compose` solutionCurve
                }
        Ok (List.map toSegment zeros.crossingCurves)

arcLengthParameterization ::
  Tolerance units =>
  Curve2D units space ->
  (Curve1D Unitless, Quantity units)
arcLengthParameterization curve =
  ArcLength.parameterization (VectorCurve2D.magnitude curve.derivative)

parameterizeByArcLength ::
  Tolerance units =>
  Curve2D units space ->
  (Curve2D units space, Quantity units)
parameterizeByArcLength curve = do
  let (parameterization, length) = arcLengthParameterization curve
  (curve `compose` parameterization, length)

makePiecewise :: NonEmpty (Curve2D units space, Quantity units) -> Curve2D units space
makePiecewise parameterizedSegments = do
  let segmentArray = Array.fromNonEmpty parameterizedSegments
  let (tree, arcLength) = buildPiecewiseTree segmentArray 0 (Array.length segmentArray)
  let evaluateImpl t = piecewiseValue tree (arcLength .*. t)
  let evaluateBoundsImpl (Interval t1 t2) =
        piecewiseBounds tree (arcLength .*. t1) (arcLength .*. t2)
  new
    (CompiledFunction.abstract evaluateImpl evaluateBoundsImpl)
    (piecewiseDerivative (piecewiseTreeDerivative tree arcLength) arcLength)

piecewise :: Tolerance units => NonEmpty (Curve2D units space) -> Curve2D units space
piecewise segments = makePiecewise (NonEmpty.map parameterizeByArcLength segments)

buildPiecewiseTree ::
  Array (Curve2D units space, Quantity units) ->
  Int ->
  Int ->
  (PiecewiseTree units space, Quantity units)
buildPiecewiseTree segmentArray begin end = case end - begin of
  1 -> do
    let (segment, length) = Array.get begin segmentArray
    (PiecewiseLeaf segment length, length)
  n -> assert (n >= 2) do
    let mid = begin + n `div` 2
    let (leftTree, leftLength) = buildPiecewiseTree segmentArray begin mid
    let (rightTree, rightLength) = buildPiecewiseTree segmentArray mid end
    (PiecewiseNode leftTree leftLength rightTree, leftLength .+. rightLength)

data PiecewiseTree units space where
  PiecewiseNode ::
    PiecewiseTree units space ->
    Quantity units ->
    PiecewiseTree units space ->
    PiecewiseTree units space
  PiecewiseLeaf ::
    Curve2D units space ->
    Quantity units ->
    PiecewiseTree units space

piecewiseValue :: PiecewiseTree units space -> Quantity units -> Point2D units space
piecewiseValue tree length = case tree of
  PiecewiseNode leftTree leftLength rightTree
    | length < leftLength -> piecewiseValue leftTree length
    | otherwise -> piecewiseValue rightTree (length .-. leftLength)
  PiecewiseLeaf curve segmentLength -> evaluate curve (length ./. segmentLength)

piecewiseBounds ::
  PiecewiseTree units space ->
  Quantity units ->
  Quantity units ->
  Bounds2D units space
piecewiseBounds tree startLength endLength = case tree of
  PiecewiseNode leftTree leftLength rightTree
    | endLength <= leftLength ->
        piecewiseBounds leftTree startLength endLength
    | startLength >= leftLength ->
        piecewiseBounds rightTree (startLength .-. leftLength) (endLength .-. leftLength)
    | otherwise ->
        Bounds2D.aggregate2
          (piecewiseBounds leftTree startLength leftLength)
          (piecewiseBounds rightTree Quantity.zero (endLength .-. leftLength))
  PiecewiseLeaf curve segmentLength ->
    evaluateBounds curve (Interval (startLength ./. segmentLength) (endLength ./. segmentLength))

piecewiseDerivative ::
  PiecewiseDerivativeTree units space ->
  Quantity units ->
  VectorCurve2D units space
piecewiseDerivative tree length = do
  let evaluateImpl t = piecewiseDerivativeValue tree (length .*. t)
  let evaluateBoundsImpl (Interval t1 t2) =
        piecewiseDerivativeBounds tree (length .*. t1) (length .*. t2)
  VectorCurve2D.new
    (CompiledFunction.abstract evaluateImpl evaluateBoundsImpl)
    (piecewiseDerivative (piecewiseDerivativeTreeDerivative tree length) length)

data PiecewiseDerivativeTree units space where
  PiecewiseDerivativeNode ::
    PiecewiseDerivativeTree units space ->
    Quantity units ->
    PiecewiseDerivativeTree units space ->
    PiecewiseDerivativeTree units space
  PiecewiseDerivativeLeaf ::
    VectorCurve2D units space ->
    Quantity units ->
    PiecewiseDerivativeTree units space

piecewiseTreeDerivative ::
  PiecewiseTree units space ->
  Quantity units ->
  PiecewiseDerivativeTree units space
piecewiseTreeDerivative tree length = case tree of
  PiecewiseNode leftTree leftLength rightTree ->
    PiecewiseDerivativeNode
      (piecewiseTreeDerivative leftTree length)
      leftLength
      (piecewiseTreeDerivative rightTree length)
  PiecewiseLeaf curve segmentLength ->
    PiecewiseDerivativeLeaf ((length ./. segmentLength) .*. curve.derivative) segmentLength

piecewiseDerivativeTreeDerivative ::
  PiecewiseDerivativeTree units space ->
  Quantity units ->
  PiecewiseDerivativeTree units space
piecewiseDerivativeTreeDerivative tree length = case tree of
  PiecewiseDerivativeNode leftTree leftLength rightTree ->
    PiecewiseDerivativeNode
      (piecewiseDerivativeTreeDerivative leftTree length)
      leftLength
      (piecewiseDerivativeTreeDerivative rightTree length)
  PiecewiseDerivativeLeaf curve segmentLength ->
    PiecewiseDerivativeLeaf
      ((length ./. segmentLength) .*. VectorCurve2D.derivative curve)
      segmentLength

piecewiseDerivativeValue ::
  PiecewiseDerivativeTree units space ->
  Quantity units ->
  Vector2D units space
piecewiseDerivativeValue tree length = case tree of
  PiecewiseDerivativeNode leftTree leftLength rightTree
    | length < leftLength -> piecewiseDerivativeValue leftTree length
    | otherwise -> piecewiseDerivativeValue rightTree (length .-. leftLength)
  PiecewiseDerivativeLeaf curve segmentLength ->
    VectorCurve2D.evaluate curve (length ./. segmentLength)

piecewiseDerivativeBounds ::
  PiecewiseDerivativeTree units space ->
  Quantity units ->
  Quantity units ->
  VectorBounds2D units space
piecewiseDerivativeBounds tree startLength endLength = case tree of
  PiecewiseDerivativeNode leftTree leftLength rightTree
    | endLength <= leftLength ->
        piecewiseDerivativeBounds leftTree startLength endLength
    | startLength >= leftLength ->
        piecewiseDerivativeBounds
          rightTree
          (startLength .-. leftLength)
          (endLength .-. leftLength)
    | otherwise ->
        VectorBounds2D.aggregate2
          (piecewiseDerivativeBounds leftTree startLength leftLength)
          (piecewiseDerivativeBounds rightTree Quantity.zero (endLength .-. leftLength))
  PiecewiseDerivativeLeaf curve segmentLength ->
    VectorCurve2D.evaluateBounds curve $
      Interval (startLength ./. segmentLength) (endLength ./. segmentLength)
