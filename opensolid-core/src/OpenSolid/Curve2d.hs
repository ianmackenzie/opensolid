module OpenSolid.Curve2d
  ( Curve2d
  , IsPoint (IsPoint)
  , Compiled
  , new
  , recursive
  , constant
  , xy
  , line
  , arc
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
import OpenSolid.Linearization qualified as Linearization
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Orientation2d (Orientation2d)
import OpenSolid.Orientation2d qualified as Orientation2d
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Polyline2d (Polyline2d (Polyline2d))
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
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d)
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d
import OpenSolid.VectorCurve3d (VectorCurve3d)
import OpenSolid.VectorCurve3d qualified as VectorCurve3d
import OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d
import OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)

-- | A parametric curve in 2D space.
data Curve2d space units = Curve2d (Compiled space units) ~(VectorCurve2d space units)

type Compiled space units =
  CompiledFunction
    Number
    (Point2d space units)
    (Bounds Unitless)
    (Bounds2d space units)

instance HasField "compiled" (Curve2d space units) (Compiled space units) where
  getField (Curve2d c _) = c

instance HasField "derivative" (Curve2d space units) (VectorCurve2d space units) where
  getField (Curve2d _ d) = d

instance HasField "secondDerivative" (Curve2d space units) (VectorCurve2d space units) where
  getField = (.derivative.derivative)

instance HasField "xCoordinate" (Curve2d space units) (Curve units) where
  getField = xCoordinate

instance HasField "yCoordinate" (Curve2d space units) (Curve units) where
  getField = yCoordinate

instance HasField "coordinates" (Curve2d space units) (Curve units, Curve units) where
  getField = coordinates

instance FFI (Curve2d FFI.Space Meters) where
  representation = FFI.classRepresentation "Curve2d"

instance FFI (Curve2d UvSpace Unitless) where
  representation = FFI.classRepresentation "UvCurve"

instance HasUnits (Curve2d space units) units

instance
  space1 ~ space2 =>
  Units.Coercion (Curve2d space1 unitsA) (Curve2d space2 unitsB)
  where
  coerce curve = Curve2d (Units.coerce curve.compiled) (Units.coerce curve.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Curve2d space1 units1) (Point2d space2 units2) units1
  where
  curve `intersects` point = (curve .-. point) `intersects` Vector2d.zero

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Point2d space1 units1) (Curve2d space2 units2) units1
  where
  point `intersects` curve = curve `intersects` point

instance ApproximateEquality (Curve2d space units) units where
  curve1 ~= curve2 = samplePoints curve1 ~= samplePoints curve2

data IsPoint = IsPoint deriving (Eq, Show)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Curve2d space1 units1)
    (VectorCurve2d space2 units2)
    (Curve2d space1 units1)
  where
  lhs .+. rhs =
    new (lhs.compiled .+. rhs.compiled) (lhs.derivative .+. rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2d space1 units1)
    (VectorCurve2d space2 units2)
    (Curve2d space1 units1)
  where
  lhs .-. rhs =
    new (lhs.compiled .-. rhs.compiled) (lhs.derivative .-. rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2d space1 units1)
    (Curve2d space2 units2)
    (VectorCurve2d space1 units1)
  where
  lhs .-. rhs =
    VectorCurve2d.new (lhs.compiled .-. rhs.compiled) (lhs.derivative .-. rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2d space1 units1)
    (Point2d space2 units2)
    (VectorCurve2d space1 units1)
  where
  curve .-. point = curve .-. constant point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d space1 units1)
    (Curve2d space2 units2)
    (VectorCurve2d space1 units1)
  where
  point .-. curve = constant point .-. curve

instance Composition (Curve Unitless) (Curve2d space units) (Curve2d space units) where
  f `compose` g =
    new (f.compiled `compose` g.compiled) ((f.derivative `compose` g) .*. g.derivative)

instance
  Composition
    (SurfaceFunction Unitless)
    (Curve2d space units)
    (SurfaceFunction2d space units)
  where
  curve `compose` function =
    SurfaceFunction2d.new
      (curve.compiled `compose` function.compiled)
      (\p -> curve.derivative `compose` function .*. SurfaceFunction.derivative p function)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (Curve2d uvSpace unitless)
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
    (Curve2d uvSpace unitless)
    (VectorSurfaceFunction3d space units)
    (VectorCurve3d space units)
  where
  function `compose` uvCurve = do
    let (dudt, dvdt) = uvCurve.derivative.components
    VectorCurve3d.new
      (function.compiled `compose` uvCurve.compiled)
      (function.du `compose` uvCurve .*. dudt .+. function.dv `compose` uvCurve .*. dvdt)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (Curve2d uvSpace unitless)
    (SurfaceFunction3d space)
    (Curve3d space)
  where
  function `compose` uvCurve = do
    let (dudt, dvdt) = uvCurve.derivative.components
    Curve3d.new
      (function.compiled `compose` uvCurve.compiled)
      (function.du `compose` uvCurve .*. dudt .+. function.dv `compose` uvCurve .*. dvdt)

new :: Compiled space units -> VectorCurve2d space units -> Curve2d space units
new = Curve2d

recursive ::
  Compiled space units ->
  (Curve2d space units -> VectorCurve2d space units) ->
  Curve2d space units
recursive givenCompiled derivativeFunction =
  let result = new givenCompiled (derivativeFunction result) in result

-- | Create a degenerate curve that is actually just a single point.
constant :: Point2d space units -> Curve2d space units
constant point = new (CompiledFunction.constant point) VectorCurve2d.zero

-- | Create a curve from its X and Y coordinate curves.
xy :: Curve units -> Curve units -> Curve2d space units
xy x y = do
  let compiledXY =
        CompiledFunction.map2
          Expression.xy
          Point2d
          Bounds2d
          x.compiled
          y.compiled
  new compiledXY (VectorCurve2d.xy x.derivative y.derivative)

-- | Create a line between two points.
line :: Point2d space units -> Point2d space units -> Curve2d space units
line p1 p2 = bezier (NonEmpty.two p1 p2)

{-| Create an arc with the given start point, end point and swept angle.

A positive swept angle means the arc turns counterclockwise (turns to the left),
and a negative swept angle means it turns clockwise (turns to the right).
For example, an arc with a swept angle of positive 90 degrees
is quarter circle that turns to the left.
-}
arc ::
  Tolerance units =>
  Point2d space units ->
  Point2d space units ->
  Angle ->
  Curve2d space units
arc givenStartPoint givenEndPoint sweptAngle =
  case Vector2d.magnitudeAndDirection (givenEndPoint .-. givenStartPoint) of
    Error Vector2d.IsZero -> line givenStartPoint givenEndPoint
    Ok (distanceBetweenPoints, directionBetweenPoints) -> do
      let halfDistance = 0.5 *. distanceBetweenPoints
      let tanHalfAngle = Angle.tan (0.5 *. sweptAngle)
      let linearDeviation = halfDistance .*. tanHalfAngle
      if linearDeviation ~= Quantity.zero
        then line givenStartPoint givenEndPoint
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
  "centerPoint" ::: Point2d space units ->
  "radius" ::: Quantity units ->
  "startAngle" ::: Angle ->
  "endAngle" ::: Angle ->
  Curve2d space units
polarArc (Named centerPoint) (Named radius) (Named startAngle) (Named endAngle) =
  customArc centerPoint (Vector2d.x radius) (Vector2d.y radius) startAngle endAngle

{-| Create an arc with the given center point, start point and swept angle.

The start point will be swept around the center point by the given angle.
-}
sweptArc :: Point2d space units -> Point2d space units -> Angle -> Curve2d space units
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
  Point2d space units ->
  "incoming" ::: Direction2d space ->
  "outgoing" ::: Direction2d space ->
  "radius" ::: Quantity units ->
  Curve2d space units
cornerArc cornerPoint (Named incomingDirection) (Named outgoingDirection) (Named givenRadius) = do
  let radius = Quantity.abs givenRadius
  let sweptAngle = Direction2d.angleFrom incomingDirection outgoingDirection
  if 0.25 *. radius .*. Number.squared (Angle.inRadians sweptAngle) ~= Quantity.zero
    then line cornerPoint cornerPoint
    else do
      let offset = radius .*. Number.abs (Angle.tan (0.5 *. sweptAngle))
      let computedStartPoint = cornerPoint .-. offset .*. incomingDirection
      let computedEndPoint = cornerPoint .+. offset .*. outgoingDirection
      arc computedStartPoint computedEndPoint sweptAngle

data WhichArc
  = SmallCounterclockwise
  | SmallClockwise
  | LargeCounterclockwise
  | LargeClockwise

radiusArc ::
  Tolerance units =>
  Quantity units ->
  Point2d space units ->
  Point2d space units ->
  WhichArc ->
  Curve2d space units
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
      line givenStartPoint givenEndPoint

ellipticalArc ::
  Frame2d space units defines ->
  Quantity units ->
  Quantity units ->
  Angle ->
  Angle ->
  Curve2d space units
ellipticalArc axes xRadius yRadius startAngle endAngle = do
  let centerPoint = Frame2d.originPoint axes
  let xVector = xRadius .*. Frame2d.xDirection axes
  let yVector = yRadius .*. Frame2d.yDirection axes
  customArc centerPoint xVector yVector startAngle endAngle

customArc ::
  Point2d space units ->
  Vector2d space units ->
  Vector2d space units ->
  Angle ->
  Angle ->
  Curve2d space units
customArc p0 v1 v2 a b = do
  let angle = Curve.line a b
  p0 .+. v1 .*. Curve.cos angle .+. v2 .*. Curve.sin angle

-- | Create a circle with the given center point and diameter.
circle ::
  "centerPoint" ::: Point2d space units ->
  "diameter" ::: Quantity units ->
  Curve2d space units
circle (Named centerPoint) (Named diameter) =
  polarArc
    (#centerPoint centerPoint)
    (#radius (0.5 *. diameter))
    (#startAngle Angle.zero)
    (#endAngle Angle.twoPi)

{-| Create an ellipes with the given principal axes and major/minor radii.
The first radius given will be the radius along the X axis,
and the second radius will be the radius along the Y axis.
-}
ellipse ::
  Frame2d space units defines ->
  Quantity units ->
  Quantity units ->
  Curve2d space units
ellipse axes xRadius yRadius = ellipticalArc axes xRadius yRadius Angle.zero Angle.twoPi

{-| Construct a Bezier curve from its control points.

For example,

> Curve2d.bezier (NonEmpty.four p1 p2 p3 p4))

will return a cubic Bezier curve with the given four control points.
-}
bezier :: NonEmpty (Point2d space units) -> Curve2d space units
bezier controlPoints =
  new
    (CompiledFunction.concrete (Expression.bezierCurve controlPoints))
    (VectorCurve2d.bezier (Bezier.derivative controlPoints))

-- | Construct a quadratic Bezier curve from the given control points.
quadraticBezier ::
  Point2d space units ->
  Point2d space units ->
  Point2d space units ->
  Curve2d space units
quadraticBezier p1 p2 p3 = bezier (NonEmpty.three p1 p2 p3)

-- | Construct a cubic Bezier curve from the given control points.
cubicBezier ::
  Point2d space units ->
  Point2d space units ->
  Point2d space units ->
  Point2d space units ->
  Curve2d space units
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
  Point2d space units ->
  List (Vector2d space units) ->
  Point2d space units ->
  List (Vector2d space units) ->
  Curve2d space units
hermite start startDerivatives end endDerivatives =
  bezier (Bezier.hermite start startDerivatives end endDerivatives)

desingularize ::
  Maybe (Point2d space units, Vector2d space units) ->
  Curve2d space units ->
  Maybe (Point2d space units, Vector2d space units) ->
  Curve2d space units
desingularize Nothing curve Nothing = curve
desingularize startSingularity curve endSingularity = do
  let startCurve = case startSingularity of
        Nothing -> curve
        Just (value0, firstDerivative0) -> do
          let t0 = Desingularization.t0
          let valueT0 = evaluate curve t0
          let firstDerivativeT0 = VectorCurve2d.evaluate curve.derivative t0
          let secondDerivativeT0 = VectorCurve2d.evaluate curve.derivative.derivative t0
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
          let firstDerivativeT1 = VectorCurve2d.evaluate curve.derivative t1
          let secondDerivativeT1 = VectorCurve2d.evaluate curve.derivative.derivative t1
          bezier $
            Bezier.syntheticEnd
              valueT1
              firstDerivativeT1
              secondDerivativeT1
              value1
              firstDerivative1
  desingularized startCurve curve endCurve

desingularized ::
  Curve2d space units ->
  Curve2d space units ->
  Curve2d space units ->
  Curve2d space units
desingularized start middle end =
  new
    (CompiledFunction.desingularized Curve.t.compiled start.compiled middle.compiled end.compiled)
    (VectorCurve2d.desingularized start.derivative middle.derivative end.derivative)

instance HasField "startPoint" (Curve2d space units) (Point2d space units) where
  getField curve = evaluate curve 0

instance HasField "endPoint" (Curve2d space units) (Point2d space units) where
  getField curve = evaluate curve 1

{-| Evaluate a curve at a given parameter value.

The parameter value should be between 0 and 1.
-}
evaluate :: Curve2d space units -> Number -> Point2d space units
evaluate curve tValue = CompiledFunction.evaluate curve.compiled tValue

{-# INLINE evaluateAt #-}
evaluateAt :: Number -> Curve2d space units -> Point2d space units
evaluateAt tValue curve = evaluate curve tValue

-- | Get the start point of a curve.
startPoint :: Curve2d space units -> Point2d space units
startPoint curve = evaluate curve 0

-- | Get the end point of a curve.
endPoint :: Curve2d space units -> Point2d space units
endPoint curve = evaluate curve 1

-- | Get the start and end points of a curve.
endpoints :: Curve2d space units -> (Point2d space units, Point2d space units)
endpoints curve = (startPoint curve, endPoint curve)

evaluateBounds :: Curve2d space units -> Bounds Unitless -> Bounds2d space units
evaluateBounds curve tBounds = CompiledFunction.evaluateBounds curve.compiled tBounds

samplePoints :: Curve2d space units -> List (Point2d space units)
samplePoints curve = List.map (evaluate curve) Parameter.samples

-- | Reverse a curve, so that the start point is the end point and vice versa.
reverse :: Curve2d space units -> Curve2d space units
reverse curve = curve `compose` (1 -. Curve.t)

bounds :: Curve2d space units -> Bounds2d space units
bounds curve = evaluateBounds curve Bounds.unitInterval

tangentDirection ::
  Tolerance units =>
  Curve2d space units ->
  Result IsPoint (DirectionCurve2d space)
tangentDirection curve =
  case VectorCurve2d.direction curve.derivative of
    Ok directionCurve -> Ok directionCurve
    Error VectorCurve2d.IsZero -> Error IsPoint

offsetLeftwardBy ::
  Tolerance units =>
  Quantity units ->
  Curve2d space units ->
  Result IsPoint (Curve2d space units)
offsetLeftwardBy offset curve = do
  tangentCurve <- tangentDirection curve
  let offsetCurve = VectorCurve2d.rotateBy Angle.quarterTurn (offset .*. tangentCurve)
  Ok (curve .+. offsetCurve)

offsetRightwardBy ::
  Tolerance units =>
  Quantity units ->
  Curve2d space units ->
  Result IsPoint (Curve2d space units)
offsetRightwardBy distance = offsetLeftwardBy (negative distance)

distanceAlong :: Axis2d space units -> Curve2d space units -> Curve units
distanceAlong (Axis2d p0 d) curve = (curve .-. p0) `dot` d

distanceLeftOf :: Axis2d space units -> Curve2d space units -> Curve units
distanceLeftOf (Axis2d p0 d) curve = (curve .-. p0) `dot` Direction2d.rotateLeft d

distanceRightOf :: Axis2d space units -> Curve2d space units -> Curve units
distanceRightOf (Axis2d p0 d) curve = (curve .-. p0) `dot` Direction2d.rotateRight d

isPoint :: Tolerance units => Curve2d space units -> Bool
isPoint curve = VectorCurve2d.isZero curve.derivative

{-| Check if the given curve curve is collinear with (lies on) the given axis.

If the curve merely intersects/touches the axis at one or more points,
then it is not considered to lie on the axis;
it is only considered to lie on the axis if every point on the curve is also on the axis.
-}
isOnAxis :: Tolerance units => Axis2d space units -> Curve2d space units -> Bool
isOnAxis axis curve = List.allSatisfy (intersects axis) (samplePoints curve)

-- | Get the X coordinate of a 2D curve as a scalar curve.
xCoordinate :: Curve2d space units -> Curve units
xCoordinate curve = do
  let compiledXCoordinate =
        CompiledFunction.map
          Expression.xCoordinate
          Point2d.xCoordinate
          Bounds2d.xCoordinate
          curve.compiled
  Curve.new compiledXCoordinate (VectorCurve2d.xComponent curve.derivative)

-- | Get the Y coordinate of a 2D curve as a scalar curve.
yCoordinate :: Curve2d space units -> Curve units
yCoordinate curve = do
  let compiledYCoordinate =
        CompiledFunction.map
          Expression.yCoordinate
          Point2d.yCoordinate
          Bounds2d.yCoordinate
          curve.compiled
  Curve.new compiledYCoordinate (VectorCurve2d.yComponent curve.derivative)

coordinates :: Curve2d space units -> (Curve units, Curve units)
coordinates curve = (xCoordinate curve, yCoordinate curve)

findPoint ::
  Tolerance units =>
  Point2d space units ->
  Curve2d space units ->
  Result IsPoint (List Number)
findPoint point curve =
  case VectorCurve2d.zeros (point .-. curve) of
    Error VectorCurve2d.IsZero -> Error IsPoint
    Ok parameterValues -> Ok parameterValues

g2 ::
  Tolerance units =>
  (Curve2d space units, Number) ->
  (Curve2d space units, Number) ->
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
  Curve2d space units ->
  Number ->
  Quantity units ->
  (Quantity units, Quantity units)
signature orientation curve tValue radius = do
  let local vector = Vector2d.relativeToOrientation orientation vector
  let firstDerivativeCurve = curve.derivative
  let secondDerivativeCurve = firstDerivativeCurve.derivative
  let firstDerivativeValue = VectorCurve2d.evaluate firstDerivativeCurve tValue
  let secondDerivativeValue = VectorCurve2d.evaluate secondDerivativeCurve tValue
  let Vector2d x' y' = local firstDerivativeValue
  let Vector2d x'' y'' = local secondDerivativeValue
  let dydx = if x' != Quantity.zero then y' ./. x' else y'' ./. x''
  let firstOrder = dydx .*. radius
  let d2ydx2 =
        if x' != Quantity.zero
          then (y'' ?*? x' .-. y' ?*? x'') ?/? (x' ?*? x' ?*? x')
          else do
            let fourthDerivativeCurve = secondDerivativeCurve.derivative.derivative
            let fourthDerivativeValue = VectorCurve2d.evaluate fourthDerivativeCurve tValue
            let Vector2d x'''' y'''' = local fourthDerivativeValue
            (y'''' ?*? x'' .-. y'' ?*? x'''') ?/? (x'' ?*? x'' ?*? x'')
  let secondOrder = Units.simplify (0.5 *. d2ydx2 ?*? Quantity.squared_ radius)
  (firstOrder, secondOrder)

placeIn ::
  Frame2d global units (Defines local) ->
  Curve2d local units ->
  Curve2d global units
placeIn frame curve = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.Curve2d.placeIn frame)
          (Point2d.placeIn frame)
          (Bounds2d.placeIn frame)
          curve.compiled
  new compiledPlaced (VectorCurve2d.placeIn frame curve.derivative)

relativeTo ::
  Frame2d global units (Defines local) ->
  Curve2d global units ->
  Curve2d local units
relativeTo frame = placeIn (Frame2d.inverse frame)

placeOn :: Plane3d space (Defines local) -> Curve2d local Meters -> Curve3d space
placeOn plane curve = Curve3d.on plane curve

transformBy ::
  Transform2d tag space units ->
  Curve2d space units ->
  Curve2d space units
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
  Vector2d space units ->
  Curve2d space units ->
  Curve2d space units
translateBy = Transform2d.translateByImpl transformBy

-- | Translate in the given direction by the given distance.
translateIn ::
  Direction2d space ->
  Quantity units ->
  Curve2d space units ->
  Curve2d space units
translateIn = Transform2d.translateInImpl transformBy

-- | Translate along the given axis by the given distance.
translateAlong ::
  Axis2d space units ->
  Quantity units ->
  Curve2d space units ->
  Curve2d space units
translateAlong = Transform2d.translateAlongImpl transformBy

-- | Rotate around the given point by the given angle.
rotateAround ::
  Point2d space units ->
  Angle ->
  Curve2d space units ->
  Curve2d space units
rotateAround = Transform2d.rotateAroundImpl transformBy

-- | Mirror across the given axis.
mirrorAcross ::
  Axis2d space units ->
  Curve2d space units ->
  Curve2d space units
mirrorAcross = Transform2d.mirrorAcrossImpl transformBy

-- | Scale uniformly about the given point by the given scaling factor.
scaleAbout ::
  Point2d space units ->
  Number ->
  Curve2d space units ->
  Curve2d space units
scaleAbout = Transform2d.scaleAboutImpl transformBy

-- | Scale (stretch) along the given axis by the given scaling factor.
scaleAlong ::
  Axis2d space units ->
  Number ->
  Curve2d space units ->
  Curve2d space units
scaleAlong = Transform2d.scaleAlongImpl transformBy

convert ::
  Quantity (units2 ?/? units1) ->
  Curve2d space units1 ->
  Curve2d space units2
convert factor curve = Units.coerce (scaleAbout Point2d.origin (Units.erase factor) curve)

unconvert ::
  Quantity (units2 ?/? units1) ->
  Curve2d space units2 ->
  Curve2d space units1
unconvert factor curve = convert (Units.simplify (1 /? factor)) curve

curvature_ ::
  Tolerance units =>
  Curve2d space units ->
  Result IsPoint (Curve (Unitless ?/? units))
curvature_ curve = do
  let firstDerivative = curve.derivative
  let secondDerivative = firstDerivative.derivative
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
  Curve2d space units1 ->
  Result IsPoint (Curve units2)
curvature curve = Result.map Units.specialize (curvature_ curve)

toPolyline :: Resolution units -> Curve2d space units -> Polyline2d (Point2d space units)
toPolyline resolution curve =
  Polyline2d (NonEmpty.map (evaluate curve) (samplingPoints resolution curve))

samplingPoints :: Resolution units -> Curve2d space units -> NonEmpty Number
samplingPoints resolution curve = do
  let size subdomain = do
        let start = evaluate curve subdomain.lower
        let end = evaluate curve subdomain.upper
        Point2d.distanceFrom start end
  let error subdomain = do
        let secondDerivativeBounds = VectorCurve2d.evaluateBounds curve.secondDerivative subdomain
        let secondDerivativeMagnitude = VectorBounds2d.magnitude secondDerivativeBounds
        Linearization.error secondDerivativeMagnitude subdomain
  let predicate = Resolution.predicate (#size size) (#error error) resolution
  Domain1d.samplingPoints predicate

medialAxis ::
  forall space units.
  Tolerance units =>
  Curve2d space units ->
  Curve2d space units ->
  Result IsPoint (List (MedialAxis.Segment space units))
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
        let curve :: SurfaceFunction2d space units =
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
  Curve2d space units ->
  (Curve Unitless, Quantity units)
arcLengthParameterization curve =
  ArcLength.parameterization (VectorCurve2d.magnitude curve.derivative)

parameterizeByArcLength ::
  Tolerance units =>
  Curve2d space units ->
  (Curve2d space units, Quantity units)
parameterizeByArcLength curve = do
  let (parameterization, length) = arcLengthParameterization curve
  (curve `compose` parameterization, length)

makePiecewise :: NonEmpty (Curve2d space units, Quantity units) -> Curve2d space units
makePiecewise parameterizedSegments = do
  let segmentArray = Array.fromNonEmpty parameterizedSegments
  let (tree, arcLength) = buildPiecewiseTree segmentArray 0 (Array.length segmentArray)
  let evaluateImpl t = piecewiseValue tree (arcLength .*. t)
  let evaluateBoundsImpl (Bounds t1 t2) =
        piecewiseBounds tree (arcLength .*. t1) (arcLength .*. t2)
  new
    (CompiledFunction.abstract evaluateImpl evaluateBoundsImpl)
    (piecewiseDerivative (piecewiseTreeDerivative tree arcLength) arcLength)

piecewise :: Tolerance units => NonEmpty (Curve2d space units) -> Curve2d space units
piecewise segments = makePiecewise (NonEmpty.map parameterizeByArcLength segments)

buildPiecewiseTree ::
  Array (Curve2d space units, Quantity units) ->
  Int ->
  Int ->
  (PiecewiseTree space units, Quantity units)
buildPiecewiseTree segmentArray begin end = case end - begin of
  1 -> do
    let (segment, length) = Array.get begin segmentArray
    (PiecewiseLeaf segment length, length)
  n -> assert (n >= 2) do
    let mid = begin + n `div` 2
    let (leftTree, leftLength) = buildPiecewiseTree segmentArray begin mid
    let (rightTree, rightLength) = buildPiecewiseTree segmentArray mid end
    (PiecewiseNode leftTree leftLength rightTree, leftLength .+. rightLength)

data PiecewiseTree space units where
  PiecewiseNode ::
    PiecewiseTree space units ->
    Quantity units ->
    PiecewiseTree space units ->
    PiecewiseTree space units
  PiecewiseLeaf ::
    Curve2d space units ->
    Quantity units ->
    PiecewiseTree space units

piecewiseValue :: PiecewiseTree space units -> Quantity units -> Point2d space units
piecewiseValue tree length = case tree of
  PiecewiseNode leftTree leftLength rightTree
    | length < leftLength -> piecewiseValue leftTree length
    | otherwise -> piecewiseValue rightTree (length .-. leftLength)
  PiecewiseLeaf curve segmentLength -> evaluate curve (length ./. segmentLength)

piecewiseBounds ::
  PiecewiseTree space units ->
  Quantity units ->
  Quantity units ->
  Bounds2d space units
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
  PiecewiseDerivativeTree space units ->
  Quantity units ->
  VectorCurve2d space units
piecewiseDerivative tree length = do
  let evaluateImpl t = piecewiseDerivativeValue tree (length .*. t)
  let evaluateBoundsImpl (Bounds t1 t2) =
        piecewiseDerivativeBounds tree (length .*. t1) (length .*. t2)
  VectorCurve2d.new
    (CompiledFunction.abstract evaluateImpl evaluateBoundsImpl)
    (piecewiseDerivative (piecewiseDerivativeTreeDerivative tree length) length)

data PiecewiseDerivativeTree space units where
  PiecewiseDerivativeNode ::
    PiecewiseDerivativeTree space units ->
    Quantity units ->
    PiecewiseDerivativeTree space units ->
    PiecewiseDerivativeTree space units
  PiecewiseDerivativeLeaf ::
    VectorCurve2d space units ->
    Quantity units ->
    PiecewiseDerivativeTree space units

piecewiseTreeDerivative ::
  PiecewiseTree space units ->
  Quantity units ->
  PiecewiseDerivativeTree space units
piecewiseTreeDerivative tree length = case tree of
  PiecewiseNode leftTree leftLength rightTree ->
    PiecewiseDerivativeNode
      (piecewiseTreeDerivative leftTree length)
      leftLength
      (piecewiseTreeDerivative rightTree length)
  PiecewiseLeaf curve segmentLength ->
    PiecewiseDerivativeLeaf ((length ./. segmentLength) .*. curve.derivative) segmentLength

piecewiseDerivativeTreeDerivative ::
  PiecewiseDerivativeTree space units ->
  Quantity units ->
  PiecewiseDerivativeTree space units
piecewiseDerivativeTreeDerivative tree length = case tree of
  PiecewiseDerivativeNode leftTree leftLength rightTree ->
    PiecewiseDerivativeNode
      (piecewiseDerivativeTreeDerivative leftTree length)
      leftLength
      (piecewiseDerivativeTreeDerivative rightTree length)
  PiecewiseDerivativeLeaf curve segmentLength ->
    PiecewiseDerivativeLeaf
      ((length ./. segmentLength) .*. curve.derivative)
      segmentLength

piecewiseDerivativeValue ::
  PiecewiseDerivativeTree space units ->
  Quantity units ->
  Vector2d space units
piecewiseDerivativeValue tree length = case tree of
  PiecewiseDerivativeNode leftTree leftLength rightTree
    | length < leftLength -> piecewiseDerivativeValue leftTree length
    | otherwise -> piecewiseDerivativeValue rightTree (length .-. leftLength)
  PiecewiseDerivativeLeaf curve segmentLength ->
    VectorCurve2d.evaluate curve (length ./. segmentLength)

piecewiseDerivativeBounds ::
  PiecewiseDerivativeTree space units ->
  Quantity units ->
  Quantity units ->
  VectorBounds2d space units
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
