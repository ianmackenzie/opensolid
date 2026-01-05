module OpenSolid.Curve2d
  ( Curve2d
  , IsPoint (IsPoint)
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
import OpenSolid.Axis2d (Axis2d (Axis2d))
import OpenSolid.Bezier qualified as Bezier
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Circle2d (Circle2d)
import OpenSolid.Circle2d qualified as Circle2d
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d.IntersectionPoint (IntersectionPoint)
import {-# SOURCE #-} OpenSolid.Curve2d.Intersections
  ( Intersections (IntersectionPoints, OverlappingSegments)
  , intersections
  )
import OpenSolid.Curve2d.MedialAxis qualified as MedialAxis
import OpenSolid.Curve2d.OverlappingSegment (OverlappingSegment (OverlappingSegment))
import {-# SOURCE #-} OpenSolid.Curve3d (Curve3d)
import {-# SOURCE #-} OpenSolid.Curve3d qualified as Curve3d
import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.DirectionCurve2d (DirectionCurve2d)
import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Domain1d qualified as Domain1d
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Curve2d qualified as Expression.Curve2d
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Line2d (Line2d (Line2d))
import OpenSolid.Linearization qualified as Linearization
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Orientation2d (Orientation2d)
import OpenSolid.Orientation2d qualified as Orientation2d
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Polyline2d (Polyline2d (Polyline2d))
import OpenSolid.Polymorphic.Point2d (Point2d (Point2d))
import OpenSolid.Polymorphic.Point2d qualified as Point2d
import OpenSolid.Polymorphic.Vector2d (Vector2d (Vector2d))
import OpenSolid.Polymorphic.Vector2d qualified as Vector2d
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Resolution (Resolution)
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction.Zeros qualified as SurfaceFunction.Zeros
import {-# SOURCE #-} OpenSolid.SurfaceFunction2d (SurfaceFunction2d)
import {-# SOURCE #-} OpenSolid.SurfaceFunction2d qualified as SurfaceFunction2d
import {-# SOURCE #-} OpenSolid.SurfaceFunction3d (SurfaceFunction3d)
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.VectorBounds2d (VectorBounds2d)
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d
import OpenSolid.VectorCurve3d (VectorCurve3d)
import OpenSolid.VectorCurve3d qualified as VectorCurve3d
import OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d
import OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)

-- | A parametric curve in 2D space.
data Curve2d units space = Curve2d
  { compiled :: Compiled units space
  , derivative :: ~(VectorCurve2d units space)
  }

type Compiled units space =
  CompiledFunction
    Number
    (Point2d units space)
    (Bounds Unitless)
    (Bounds2d units space)

instance HasField "xCoordinate" (Curve2d units space) (Curve units) where
  getField = xCoordinate

instance HasField "yCoordinate" (Curve2d units space) (Curve units) where
  getField = yCoordinate

instance HasField "coordinates" (Curve2d units space) (Curve units, Curve units) where
  getField = coordinates

instance FFI (Curve2d Meters FFI.Space) where
  representation = FFI.classRepresentation "Curve2d"

instance FFI (Curve2d Unitless UvSpace) where
  representation = FFI.classRepresentation "UvCurve"

instance HasUnits (Curve2d units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (Curve2d unitsA space1) (Curve2d unitsB space2)
  where
  coerce curve = Curve2d (Units.coerce curve.compiled) (Units.coerce curve.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Curve2d units1 space1) (Point2d units2 space2) units1
  where
  curve `intersects` point = (curve .-. point) `intersects` Vector2d.zero

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Point2d units1 space1) (Curve2d units2 space2) units1
  where
  point `intersects` curve = curve `intersects` point

instance ApproximateEquality (Curve2d units space) units where
  curve1 ~= curve2 = samplePoints curve1 ~= samplePoints curve2

data IsPoint = IsPoint deriving (Eq, Show)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Curve2d units1 space1)
    (VectorCurve2d units2 space2)
    (Curve2d units1 space1)
  where
  lhs .+. rhs =
    new
      (compiled lhs .+. VectorCurve2d.compiled rhs)
      (derivative lhs .+. VectorCurve2d.derivative rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2d units1 space1)
    (VectorCurve2d units2 space2)
    (Curve2d units1 space1)
  where
  lhs .-. rhs =
    new
      (compiled lhs .-. VectorCurve2d.compiled rhs)
      (derivative lhs .-. VectorCurve2d.derivative rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2d units1 space1)
    (Curve2d units2 space2)
    (VectorCurve2d units1 space1)
  where
  lhs .-. rhs =
    VectorCurve2d.new (lhs.compiled .-. rhs.compiled) (lhs.derivative .-. rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2d units1 space1)
    (Point2d units2 space2)
    (VectorCurve2d units1 space1)
  where
  curve .-. point = curve .-. constant point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d units1 space1)
    (Curve2d units2 space2)
    (VectorCurve2d units1 space1)
  where
  point .-. curve = constant point .-. curve

instance Composition (Curve Unitless) (Curve2d units space) (Curve2d units space) where
  f `compose` g =
    new (f.compiled `compose` g.compiled) ((f.derivative `compose` g) .*. g.derivative)

instance
  Composition
    (SurfaceFunction Unitless)
    (Curve2d units space)
    (SurfaceFunction2d units space)
  where
  curve `compose` function =
    SurfaceFunction2d.new
      (curve.compiled `compose` function.compiled)
      (\p -> curve.derivative `compose` function .*. SurfaceFunction.derivative p function)

instance
  Composition
    SurfaceParameter
    (Curve2d units space)
    (SurfaceFunction2d units space)
  where
  curve `compose` parameter = curve `compose` SurfaceFunction.parameter parameter

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (Curve2d unitless uvSpace)
    (SurfaceFunction units)
    (Curve units)
  where
  f `compose` g = do
    let (dudt, dvdt) = g.derivative.components
    Curve.new
      (f.compiled `compose` g.compiled)
      (f.du `compose` g .*. dudt .+. f.dv `compose` g .*. dvdt)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (Curve2d unitless uvSpace)
    (VectorSurfaceFunction3d units space)
    (VectorCurve3d units space)
  where
  function `compose` uvCurve = do
    let (dudt, dvdt) = uvCurve.derivative.components
    VectorCurve3d.new
      (function.compiled `compose` uvCurve.compiled)
      (function.du `compose` uvCurve .*. dudt .+. function.dv `compose` uvCurve .*. dvdt)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (Curve2d unitless uvSpace)
    (SurfaceFunction3d space)
    (Curve3d space)
  where
  function `compose` uvCurve = do
    let (dudt, dvdt) = uvCurve.derivative.components
    Curve3d.new
      (function.compiled `compose` uvCurve.compiled)
      (function.du `compose` uvCurve .*. dudt .+. function.dv `compose` uvCurve .*. dvdt)

new :: Compiled units space -> VectorCurve2d units space -> Curve2d units space
new = Curve2d

recursive ::
  Compiled units space ->
  (Curve2d units space -> VectorCurve2d units space) ->
  Curve2d units space
recursive givenCompiled derivativeFunction =
  let result = new givenCompiled (derivativeFunction result) in result

-- | Create a degenerate curve that is actually just a single point.
constant :: Point2d units space -> Curve2d units space
constant point = new (CompiledFunction.constant point) VectorCurve2d.zero

-- | Create a curve from its X and Y coordinate curves.
xy :: Curve units -> Curve units -> Curve2d units space
xy x y = do
  let compiledXY = CompiledFunction.map2 Expression.xy Point2d Bounds2d x.compiled y.compiled
  new compiledXY (VectorCurve2d.xy x.derivative y.derivative)

-- | Convert a line to a curve.
line :: Line2d units space -> Curve2d units space
line (Line2d p1 p2) = lineFrom p1 p2

-- | Create a line between two points.
lineFrom :: Point2d units space -> Point2d units space -> Curve2d units space
lineFrom p1 p2 = bezier (NonEmpty.two p1 p2)

{-| Create an arc from the given start point to the given end point, with the given swept angle.

A positive swept angle means the arc turns counterclockwise (turns to the left),
and a negative swept angle means it turns clockwise (turns to the right).
For example, an arc with a swept angle of positive 90 degrees
is quarter circle that turns to the left.
-}
arcFrom ::
  Tolerance units =>
  Point2d units space ->
  Point2d units space ->
  Angle ->
  Curve2d units space
arcFrom givenStartPoint givenEndPoint sweptAngle =
  case Vector2d.magnitudeAndDirection (givenEndPoint .-. givenStartPoint) of
    Error Vector2d.IsZero -> lineFrom givenStartPoint givenEndPoint
    Ok (distanceBetweenPoints, directionBetweenPoints) -> do
      let halfDistance = 0.5 *. distanceBetweenPoints
      let tanHalfAngle = Angle.tan (0.5 *. sweptAngle)
      let linearDeviation = halfDistance .*. tanHalfAngle
      if linearDeviation ~= Quantity.zero
        then lineFrom givenStartPoint givenEndPoint
        else do
          let offset = (halfDistance ./. tanHalfAngle) .*. Direction2d.rotateLeft directionBetweenPoints
          let centerPoint = Point2d.midpoint givenStartPoint givenEndPoint .+. offset
          let radius = Point2d.distanceFrom centerPoint givenStartPoint
          let xVector = Vector2d.x radius
          let yVector = Vector2d.y radius
          let startAngle = Point2d.angleFrom centerPoint givenStartPoint
          let endAngle = startAngle .+. sweptAngle
          customArc centerPoint xVector yVector startAngle endAngle

-- | Create an arc with the given center point, radius, start angle and end angle.
polarArc ::
  "centerPoint" ::: Point2d units space ->
  "radius" ::: Quantity units ->
  "startAngle" ::: Angle ->
  "endAngle" ::: Angle ->
  Curve2d units space
polarArc (Named centerPoint) (Named radius) (Named startAngle) (Named endAngle) =
  customArc centerPoint (Vector2d.x radius) (Vector2d.y radius) startAngle endAngle

{-| Create an arc with the given center point, start point and swept angle.

The start point will be swept around the center point by the given angle.
-}
sweptArc :: Point2d units space -> Point2d units space -> Angle -> Curve2d units space
sweptArc centerPoint givenStartPoint sweptAngle = do
  let radius = Point2d.distanceFrom centerPoint givenStartPoint
  let startAngle = Point2d.angleFrom centerPoint givenStartPoint
  polarArc
    (#centerPoint centerPoint)
    (#radius radius)
    (#startAngle startAngle)
    (#endAngle (startAngle .+. sweptAngle))

-- | Create an arc for rounding off the corner between two straight lines.
cornerArc ::
  Tolerance units =>
  Point2d units space ->
  "incoming" ::: Direction2d space ->
  "outgoing" ::: Direction2d space ->
  "radius" ::: Quantity units ->
  Curve2d units space
cornerArc cornerPoint (Named incomingDirection) (Named outgoingDirection) (Named givenRadius) = do
  let radius = Quantity.abs givenRadius
  let sweptAngle = Direction2d.angleFrom incomingDirection outgoingDirection
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
  Point2d units space ->
  Point2d units space ->
  WhichArc ->
  Curve2d units space
radiusArc givenRadius givenStartPoint givenEndPoint whichArc =
  case Direction2d.from givenStartPoint givenEndPoint of
    Ok chordDirection -> do
      let halfDistance = 0.5 *. Point2d.distanceFrom givenStartPoint givenEndPoint
      let radius = max (Quantity.abs givenRadius) halfDistance
      let offsetMagnitude =
            Quantity.sqrt_ (Quantity.squared_ radius .-. Quantity.squared_ halfDistance)
      let offsetDirection = Direction2d.rotateLeft chordDirection
      let offsetDistance =
            case whichArc of
              SmallCounterclockwise -> offsetMagnitude
              SmallClockwise -> negative offsetMagnitude
              LargeClockwise -> offsetMagnitude
              LargeCounterclockwise -> negative offsetMagnitude
      let offset = offsetDirection .*. offsetDistance
      let centerPoint = Point2d.midpoint givenStartPoint givenEndPoint .+. offset
      let shortAngle = 2 *. Angle.asin (halfDistance ./. givenRadius)
      let sweptAngle =
            case whichArc of
              SmallCounterclockwise -> shortAngle
              SmallClockwise -> negative shortAngle
              LargeClockwise -> shortAngle .-. Angle.twoPi
              LargeCounterclockwise -> Angle.twoPi .-. shortAngle
      sweptArc centerPoint givenStartPoint sweptAngle
    Error Direction2d.PointsAreCoincident ->
      lineFrom givenStartPoint givenEndPoint

ellipticalArc ::
  Frame2d units global local ->
  Quantity units ->
  Quantity units ->
  Angle ->
  Angle ->
  Curve2d units global
ellipticalArc axes xRadius yRadius startAngle endAngle = do
  let centerPoint = Frame2d.originPoint axes
  let xVector = xRadius .*. Frame2d.xDirection axes
  let yVector = yRadius .*. Frame2d.yDirection axes
  customArc centerPoint xVector yVector startAngle endAngle

customArc ::
  Point2d units space ->
  Vector2d units space ->
  Vector2d units space ->
  Angle ->
  Angle ->
  Curve2d units space
customArc p0 v1 v2 a b = do
  let angle = Curve.interpolateFrom a b
  p0 .+. v1 .*. Curve.cos angle .+. v2 .*. Curve.sin angle

-- | Create a curve from the given circle.
circle :: Circle2d units space -> Curve2d units space
circle givenCircle =
  polarArc
    (#centerPoint (Circle2d.centerPoint givenCircle))
    (#radius (Circle2d.radius givenCircle))
    (#startAngle Angle.zero)
    (#endAngle Angle.twoPi)

{-| Create an ellipes with the given principal axes and major/minor radii.
The first radius given will be the radius along the X axis,
and the second radius will be the radius along the Y axis.
-}
ellipse ::
  Frame2d units global local ->
  Quantity units ->
  Quantity units ->
  Curve2d units global
ellipse axes xRadius yRadius = ellipticalArc axes xRadius yRadius Angle.zero Angle.twoPi

{-| Construct a Bezier curve from its control points.

For example,

> Curve2d.bezier (NonEmpty.four p1 p2 p3 p4))

will return a cubic Bezier curve with the given four control points.
-}
bezier :: NonEmpty (Point2d units space) -> Curve2d units space
bezier controlPoints =
  new
    (CompiledFunction.concrete (Expression.bezierCurve controlPoints))
    (VectorCurve2d.bezier (Bezier.derivative controlPoints))

-- | Construct a quadratic Bezier curve from the given control points.
quadraticBezier ::
  Point2d units space ->
  Point2d units space ->
  Point2d units space ->
  Curve2d units space
quadraticBezier p1 p2 p3 = bezier (NonEmpty.three p1 p2 p3)

-- | Construct a cubic Bezier curve from the given control points.
cubicBezier ::
  Point2d units space ->
  Point2d units space ->
  Point2d units space ->
  Point2d units space ->
  Curve2d units space
cubicBezier p1 p2 p3 p4 = bezier (NonEmpty.four p1 p2 p3 p4)

{-| Construct a Bezier curve with the given endpoints and derivatives at those endpoints.

For example,

> Curve2d.hermite p1 [v1] p2 [v2]

will result in a cubic spline from @p1@ to @p2@ with first derivative equal to @v1@ at @p1@ and
first derivative equal to @v2@ at @p2@.

The numbers of derivatives at each endpoint do not have to be equal; for example,

> Curve2d.hermite p1 [v1] p2 []

will result in a quadratic spline from @p1@ to @p2@ with first derivative at @p1@ equal to @v1@.

In general, the degree of the resulting spline will be equal to 1 plus the total number of
derivatives given.
-}
hermite ::
  Point2d units space ->
  List (Vector2d units space) ->
  Point2d units space ->
  List (Vector2d units space) ->
  Curve2d units space
hermite start startDerivatives end endDerivatives =
  bezier (Bezier.hermite start startDerivatives end endDerivatives)

desingularize ::
  Maybe (Point2d units space, Vector2d units space) ->
  Curve2d units space ->
  Maybe (Point2d units space, Vector2d units space) ->
  Curve2d units space
desingularize Nothing curve Nothing = curve
desingularize startSingularity curve endSingularity = do
  let startCurve = case startSingularity of
        Nothing -> curve
        Just (value0, firstDerivative0) -> do
          let t0 = Desingularization.t0
          let valueT0 = evaluate curve t0
          let firstDerivative = derivative curve
          let secondDerivative = VectorCurve2d.derivative firstDerivative
          let firstDerivativeT0 = VectorCurve2d.evaluate firstDerivative t0
          let secondDerivativeT0 = VectorCurve2d.evaluate secondDerivative t0
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
          let firstDerivative = derivative curve
          let secondDerivative = VectorCurve2d.derivative firstDerivative
          let firstDerivativeT1 = VectorCurve2d.evaluate firstDerivative t1
          let secondDerivativeT1 = VectorCurve2d.evaluate secondDerivative t1
          bezier $
            Bezier.syntheticEnd
              valueT1
              firstDerivativeT1
              secondDerivativeT1
              value1
              firstDerivative1
  desingularized startCurve curve endCurve

desingularized ::
  Curve2d units space ->
  Curve2d units space ->
  Curve2d units space ->
  Curve2d units space
desingularized start middle end =
  new
    (CompiledFunction.desingularized Curve.t.compiled start.compiled middle.compiled end.compiled)
    (VectorCurve2d.desingularized start.derivative middle.derivative end.derivative)

instance HasField "startPoint" (Curve2d units space) (Point2d units space) where
  getField curve = evaluate curve 0

instance HasField "endPoint" (Curve2d units space) (Point2d units space) where
  getField curve = evaluate curve 1

{-| Evaluate a curve at a given parameter value.

The parameter value should be between 0 and 1.
-}
evaluate :: Curve2d units space -> Number -> Point2d units space
evaluate curve tValue = CompiledFunction.evaluate curve.compiled tValue

{-# INLINE evaluateAt #-}
evaluateAt :: Number -> Curve2d units space -> Point2d units space
evaluateAt tValue curve = evaluate curve tValue

-- | Get the start point of a curve.
startPoint :: Curve2d units space -> Point2d units space
startPoint curve = evaluate curve 0

-- | Get the end point of a curve.
endPoint :: Curve2d units space -> Point2d units space
endPoint curve = evaluate curve 1

-- | Get the start and end points of a curve.
endpoints :: Curve2d units space -> (Point2d units space, Point2d units space)
endpoints curve = (startPoint curve, endPoint curve)

evaluateBounds :: Curve2d units space -> Bounds Unitless -> Bounds2d units space
evaluateBounds curve tBounds = CompiledFunction.evaluateBounds curve.compiled tBounds

samplePoints :: Curve2d units space -> List (Point2d units space)
samplePoints curve = List.map (evaluate curve) Parameter.samples

-- | Reverse a curve, so that the start point is the end point and vice versa.
reverse :: Curve2d units space -> Curve2d units space
reverse curve = curve `compose` (1 -. Curve.t)

bounds :: Curve2d units space -> Bounds2d units space
bounds curve = evaluateBounds curve Bounds.unitInterval

compiled :: Curve2d units space -> Compiled units space
compiled = (.compiled)

derivative :: Curve2d units space -> VectorCurve2d units space
derivative = (.derivative)

tangentDirection ::
  Tolerance units =>
  Curve2d units space ->
  Result IsPoint (DirectionCurve2d space)
tangentDirection curve =
  case VectorCurve2d.direction curve.derivative of
    Ok directionCurve -> Ok directionCurve
    Error VectorCurve2d.IsZero -> Error IsPoint

offsetLeftwardBy ::
  Tolerance units =>
  Quantity units ->
  Curve2d units space ->
  Result IsPoint (Curve2d units space)
offsetLeftwardBy offset curve = do
  tangentCurve <- tangentDirection curve
  let offsetCurve = VectorCurve2d.rotateBy Angle.quarterTurn (offset .*. tangentCurve)
  Ok (curve .+. offsetCurve)

offsetRightwardBy ::
  Tolerance units =>
  Quantity units ->
  Curve2d units space ->
  Result IsPoint (Curve2d units space)
offsetRightwardBy distance = offsetLeftwardBy (negative distance)

distanceAlong :: Axis2d units space -> Curve2d units space -> Curve units
distanceAlong (Axis2d p0 d) curve = (curve .-. p0) `dot` d

distanceLeftOf :: Axis2d units space -> Curve2d units space -> Curve units
distanceLeftOf (Axis2d p0 d) curve = (curve .-. p0) `dot` Direction2d.rotateLeft d

distanceRightOf :: Axis2d units space -> Curve2d units space -> Curve units
distanceRightOf (Axis2d p0 d) curve = (curve .-. p0) `dot` Direction2d.rotateRight d

isPoint :: Tolerance units => Curve2d units space -> Bool
isPoint curve = VectorCurve2d.isZero curve.derivative

{-| Check if the given curve curve is collinear with (lies on) the given axis.

If the curve merely intersects/touches the axis at one or more points,
then it is not considered to lie on the axis;
it is only considered to lie on the axis if every point on the curve is also on the axis.
-}
isOnAxis :: Tolerance units => Axis2d units space -> Curve2d units space -> Bool
isOnAxis axis curve = List.allSatisfy (intersects axis) (samplePoints curve)

-- | Get the X coordinate of a 2D curve as a scalar curve.
xCoordinate :: Curve2d units space -> Curve units
xCoordinate curve = do
  let compiledXCoordinate =
        CompiledFunction.map
          Expression.xCoordinate
          Point2d.xCoordinate
          Bounds2d.xCoordinate
          curve.compiled
  Curve.new compiledXCoordinate (VectorCurve2d.xComponent curve.derivative)

-- | Get the Y coordinate of a 2D curve as a scalar curve.
yCoordinate :: Curve2d units space -> Curve units
yCoordinate curve = do
  let compiledYCoordinate =
        CompiledFunction.map
          Expression.yCoordinate
          Point2d.yCoordinate
          Bounds2d.yCoordinate
          curve.compiled
  Curve.new compiledYCoordinate (VectorCurve2d.yComponent curve.derivative)

coordinates :: Curve2d units space -> (Curve units, Curve units)
coordinates curve = (xCoordinate curve, yCoordinate curve)

findPoint ::
  Tolerance units =>
  Point2d units space ->
  Curve2d units space ->
  Result IsPoint (List Number)
findPoint point curve =
  case VectorCurve2d.zeros (point .-. curve) of
    Error VectorCurve2d.IsZero -> Error IsPoint
    Ok parameterValues -> Ok parameterValues

g2 ::
  Tolerance units =>
  (Curve2d units space, Number) ->
  (Curve2d units space, Number) ->
  Quantity units ->
  Bool
g2 (curve1, t1) (curve2, t2) radius =
  evaluate curve1 t1 ~= evaluate curve2 t2 && do
    let first1 = VectorCurve2d.evaluate curve1.derivative t1
    let first2 = VectorCurve2d.evaluate curve2.derivative t2
    let Vector2d dxdt1 dydt1 = first1
    let Vector2d dxdt2 dydt2 = first2
    let dxdtMin = min (Quantity.abs dxdt1) (Quantity.abs dxdt2)
    let dydtMin = min (Quantity.abs dydt1) (Quantity.abs dydt2)
    let orientation =
          if dxdtMin >= dydtMin
            then Orientation2d.horizontal
            else Orientation2d.vertical
    let signature1 = signature orientation curve1 t1 radius
    let signature2 = signature orientation curve2 t2 radius
    signature1 ~= signature2

signature ::
  Tolerance units =>
  Orientation2d space ->
  Curve2d units space ->
  Number ->
  Quantity units ->
  (Quantity units, Quantity units)
signature orientation curve tValue radius = do
  let local vector = Vector2d.relativeToOrientation orientation vector
  let firstDerivative = derivative curve
  let secondDerivative = VectorCurve2d.derivative firstDerivative
  let firstDerivativeValue = VectorCurve2d.evaluate firstDerivative tValue
  let secondDerivativeValue = VectorCurve2d.evaluate secondDerivative tValue
  let Vector2d x' y' = local firstDerivativeValue
  let Vector2d x'' y'' = local secondDerivativeValue
  let dydx = if x' != Quantity.zero then y' ./. x' else y'' ./. x''
  let firstOrder = dydx .*. radius
  let d2ydx2 =
        if x' != Quantity.zero
          then (y'' ?*? x' .-. y' ?*? x'') ?/? (x' ?*? x' ?*? x')
          else do
            let thirdDerivative = VectorCurve2d.derivative secondDerivative
            let fourthDerivative = VectorCurve2d.derivative thirdDerivative
            let fourthDerivativeValue = VectorCurve2d.evaluate fourthDerivative tValue
            let Vector2d x'''' y'''' = local fourthDerivativeValue
            (y'''' ?*? x'' .-. y'' ?*? x'''') ?/? (x'' ?*? x'' ?*? x'')
  let secondOrder = Units.simplify (0.5 *. d2ydx2 ?*? Quantity.squared_ radius)
  (firstOrder, secondOrder)

placeIn ::
  Frame2d units global local ->
  Curve2d units local ->
  Curve2d units global
placeIn frame curve = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.Curve2d.placeIn frame)
          (Point2d.placeIn frame)
          (Bounds2d.placeIn frame)
          curve.compiled
  new compiledPlaced (VectorCurve2d.placeIn frame curve.derivative)

relativeTo ::
  Frame2d units global local ->
  Curve2d units global ->
  Curve2d units local
relativeTo frame = placeIn (Frame2d.inverse frame)

placeOn :: Plane3d global local -> Curve2d Meters local -> Curve3d global
placeOn plane curve = Curve3d.on plane curve

transformBy ::
  Transform2d tag units space ->
  Curve2d units space ->
  Curve2d units space
transformBy transform curve = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.Curve2d.transformBy transform)
          (Point2d.transformBy transform)
          (Bounds2d.transformBy transform)
          curve.compiled
  new compiledTransformed (VectorCurve2d.transformBy transform curve.derivative)

-- | Translate by the given displacement.
translateBy ::
  Vector2d units space ->
  Curve2d units space ->
  Curve2d units space
translateBy = Transform2d.translateByImpl transformBy

-- | Translate in the given direction by the given distance.
translateIn ::
  Direction2d space ->
  Quantity units ->
  Curve2d units space ->
  Curve2d units space
translateIn = Transform2d.translateInImpl transformBy

-- | Translate along the given axis by the given distance.
translateAlong ::
  Axis2d units space ->
  Quantity units ->
  Curve2d units space ->
  Curve2d units space
translateAlong = Transform2d.translateAlongImpl transformBy

-- | Rotate around the given point by the given angle.
rotateAround ::
  Point2d units space ->
  Angle ->
  Curve2d units space ->
  Curve2d units space
rotateAround = Transform2d.rotateAroundImpl transformBy

-- | Mirror across the given axis.
mirrorAcross ::
  Axis2d units space ->
  Curve2d units space ->
  Curve2d units space
mirrorAcross = Transform2d.mirrorAcrossImpl transformBy

-- | Scale uniformly about the given point by the given scaling factor.
scaleAbout ::
  Point2d units space ->
  Number ->
  Curve2d units space ->
  Curve2d units space
scaleAbout = Transform2d.scaleAboutImpl transformBy

-- | Scale (stretch) along the given axis by the given scaling factor.
scaleAlong ::
  Axis2d units space ->
  Number ->
  Curve2d units space ->
  Curve2d units space
scaleAlong = Transform2d.scaleAlongImpl transformBy

convert ::
  Quantity (units2 ?/? units1) ->
  Curve2d units1 space ->
  Curve2d units2 space
convert factor curve = Units.coerce (scaleAbout Point2d.origin (Units.erase factor) curve)

unconvert ::
  Quantity (units2 ?/? units1) ->
  Curve2d units2 space ->
  Curve2d units1 space
unconvert factor curve = convert (Units.simplify (1 /? factor)) curve

curvature_ ::
  Tolerance units =>
  Curve2d units space ->
  Result IsPoint (Curve (Unitless ?/? units))
curvature_ curve = do
  let firstDerivative = derivative curve
  let secondDerivative = VectorCurve2d.derivative firstDerivative
  tangent <- tangentDirection curve
  let numerator = tangent `cross` secondDerivative
  let denominator = VectorCurve2d.squaredMagnitude_ firstDerivative
  case Tolerance.using (Quantity.squared_ ?tolerance) (Curve.quotient_ numerator denominator) of
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
  Curve2d units1 space ->
  Result IsPoint (Curve units2)
curvature curve = Result.map Units.specialize (curvature_ curve)

toPolyline :: Resolution units -> Curve2d units space -> Polyline2d units space
toPolyline resolution curve =
  Polyline2d (NonEmpty.map (evaluate curve) (samplingPoints resolution curve))

samplingPoints :: Resolution units -> Curve2d units space -> NonEmpty Number
samplingPoints resolution curve = do
  let size subdomain = do
        let start = evaluate curve subdomain.lower
        let end = evaluate curve subdomain.upper
        Point2d.distanceFrom start end
  let firstDerivative = derivative curve
  let secondDerivative = VectorCurve2d.derivative firstDerivative
  let error subdomain = do
        let secondDerivativeBounds = VectorCurve2d.evaluateBounds secondDerivative subdomain
        let secondDerivativeMagnitude = VectorBounds2d.magnitude secondDerivativeBounds
        Linearization.error secondDerivativeMagnitude subdomain
  let predicate = Resolution.predicate (#size size) (#error error) resolution
  Domain1d.samplingPoints predicate

medialAxis ::
  forall units space.
  Tolerance units =>
  Curve2d units space ->
  Curve2d units space ->
  Result IsPoint (List (MedialAxis.Segment units space))
medialAxis curve1 curve2 = do
  let p1 = curve1 `compose` SurfaceFunction.u
  let p2 = curve2 `compose` SurfaceFunction.v
  let v1 = curve1.derivative `compose` SurfaceFunction.u
  let v2 = curve2.derivative `compose` SurfaceFunction.v
  let d = p2 .-. p1
  let target =
        v2 `cross_` (2 *. (v1 `dot_` d) ?*? d .-. VectorSurfaceFunction2d.squaredMagnitude_ d ?*? v1)
  let targetTolerance = ?tolerance ?*? ((?tolerance ?*? ?tolerance) ?*? ?tolerance)
  case Tolerance.using targetTolerance (SurfaceFunction.zeros target) of
    Error SurfaceFunction.IsZero -> TODO -- curves are identical arcs?
    Ok zeros ->
      assert (List.isEmpty zeros.crossingLoops && List.isEmpty zeros.tangentPoints) do
        tangentDirection1 <- tangentDirection curve1
        let tangentVector1 = VectorCurve2d.unit tangentDirection1
        let normal1 = VectorCurve2d.rotateBy Angle.quarterTurn tangentVector1
        let radius :: SurfaceFunction units =
              Units.coerce $
                SurfaceFunction.unsafeQuotient_
                  (d `dot_` d)
                  (2 *. (tangentVector1 `compose` SurfaceFunction.u) `cross` d)
        let curve :: SurfaceFunction2d units space =
              (curve1 `compose` SurfaceFunction.u) .+. radius .*. (normal1 `compose` SurfaceFunction.u)
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
  Curve2d units space ->
  (Curve Unitless, Quantity units)
arcLengthParameterization curve =
  ArcLength.parameterization (VectorCurve2d.magnitude curve.derivative)

parameterizeByArcLength ::
  Tolerance units =>
  Curve2d units space ->
  (Curve2d units space, Quantity units)
parameterizeByArcLength curve = do
  let (parameterization, length) = arcLengthParameterization curve
  (curve `compose` parameterization, length)

makePiecewise :: NonEmpty (Curve2d units space, Quantity units) -> Curve2d units space
makePiecewise parameterizedSegments = do
  let segmentArray = Array.fromNonEmpty parameterizedSegments
  let (tree, arcLength) = buildPiecewiseTree segmentArray 0 (Array.length segmentArray)
  let evaluateImpl t = piecewiseValue tree (arcLength .*. t)
  let evaluateBoundsImpl (Bounds t1 t2) =
        piecewiseBounds tree (arcLength .*. t1) (arcLength .*. t2)
  new
    (CompiledFunction.abstract evaluateImpl evaluateBoundsImpl)
    (piecewiseDerivative (piecewiseTreeDerivative tree arcLength) arcLength)

piecewise :: Tolerance units => NonEmpty (Curve2d units space) -> Curve2d units space
piecewise segments = makePiecewise (NonEmpty.map parameterizeByArcLength segments)

buildPiecewiseTree ::
  Array (Curve2d units space, Quantity units) ->
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
    Curve2d units space ->
    Quantity units ->
    PiecewiseTree units space

piecewiseValue :: PiecewiseTree units space -> Quantity units -> Point2d units space
piecewiseValue tree length = case tree of
  PiecewiseNode leftTree leftLength rightTree
    | length < leftLength -> piecewiseValue leftTree length
    | otherwise -> piecewiseValue rightTree (length .-. leftLength)
  PiecewiseLeaf curve segmentLength -> evaluate curve (length ./. segmentLength)

piecewiseBounds ::
  PiecewiseTree units space ->
  Quantity units ->
  Quantity units ->
  Bounds2d units space
piecewiseBounds tree startLength endLength = case tree of
  PiecewiseNode leftTree leftLength rightTree
    | endLength <= leftLength ->
        piecewiseBounds leftTree startLength endLength
    | startLength >= leftLength ->
        piecewiseBounds rightTree (startLength .-. leftLength) (endLength .-. leftLength)
    | otherwise ->
        Bounds2d.aggregate2
          (piecewiseBounds leftTree startLength leftLength)
          (piecewiseBounds rightTree Quantity.zero (endLength .-. leftLength))
  PiecewiseLeaf curve segmentLength ->
    evaluateBounds curve (Bounds (startLength ./. segmentLength) (endLength ./. segmentLength))

piecewiseDerivative ::
  PiecewiseDerivativeTree units space ->
  Quantity units ->
  VectorCurve2d units space
piecewiseDerivative tree length = do
  let evaluateImpl t = piecewiseDerivativeValue tree (length .*. t)
  let evaluateBoundsImpl (Bounds t1 t2) =
        piecewiseDerivativeBounds tree (length .*. t1) (length .*. t2)
  VectorCurve2d.new
    (CompiledFunction.abstract evaluateImpl evaluateBoundsImpl)
    (piecewiseDerivative (piecewiseDerivativeTreeDerivative tree length) length)

data PiecewiseDerivativeTree units space where
  PiecewiseDerivativeNode ::
    PiecewiseDerivativeTree units space ->
    Quantity units ->
    PiecewiseDerivativeTree units space ->
    PiecewiseDerivativeTree units space
  PiecewiseDerivativeLeaf ::
    VectorCurve2d units space ->
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
      ((length ./. segmentLength) .*. VectorCurve2d.derivative curve)
      segmentLength

piecewiseDerivativeValue ::
  PiecewiseDerivativeTree units space ->
  Quantity units ->
  Vector2d units space
piecewiseDerivativeValue tree length = case tree of
  PiecewiseDerivativeNode leftTree leftLength rightTree
    | length < leftLength -> piecewiseDerivativeValue leftTree length
    | otherwise -> piecewiseDerivativeValue rightTree (length .-. leftLength)
  PiecewiseDerivativeLeaf curve segmentLength ->
    VectorCurve2d.evaluate curve (length ./. segmentLength)

piecewiseDerivativeBounds ::
  PiecewiseDerivativeTree units space ->
  Quantity units ->
  Quantity units ->
  VectorBounds2d units space
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
        VectorBounds2d.aggregate2
          (piecewiseDerivativeBounds leftTree startLength leftLength)
          (piecewiseDerivativeBounds rightTree Quantity.zero (endLength .-. leftLength))
  PiecewiseDerivativeLeaf curve segmentLength ->
    VectorCurve2d.evaluateBounds curve $
      Bounds (startLength ./. segmentLength) (endLength ./. segmentLength)
