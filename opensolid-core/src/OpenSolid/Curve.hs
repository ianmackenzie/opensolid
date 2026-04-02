{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve
  ( Curve
  , Curve2D
  , Curve3D
  , Exists
  , Compiled
  , Segment
  , SearchTree
  , HasSingularity (HasSingularity)
  , new
  , constant
  , line
  , lineFrom
  , bezier
  , quadraticBezier
  , cubicBezier
  , hermite
  , derivative
  , compiled
  , overallBounds
  , point
  , bounds
  , startPoint
  , endPoint
  , endpoints
  , secondDerivative
  , derivativeValue
  , derivativeBounds
  , secondDerivativeValue
  , secondDerivativeBounds
  , tangentDirectionBounds
  , reverse
  , isPoint
  , singular0
  , singular1
  , isOnAxis
  , nondegenerate
  , nonzero
  , tangentDirection
  , curvatureVector_
  , distanceAlong
  , desingularize
  , desingularized
  , findPoint
  , searchTree
  , Intersections (IntersectionPoints, OverlappingSegments)
  , IntersectionPoint
  , intersections
  , linearDeviation
  , linearize
  , toPolyline
  , arcLengthParameterizationFunction
  , arcLengthParameterization
  , parameterizeByArcLength
  )
where

import Data.Void (Void)
import OpenSolid.ArcLength qualified as ArcLength
import OpenSolid.Axis (Axis)
import OpenSolid.Axis qualified as Axis
import OpenSolid.Bezier qualified as Bezier
import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve.IntersectionPoint (IntersectionPoint)
import {-# SOURCE #-} OpenSolid.Curve.Intersections (Intersections)
import {-# SOURCE #-} OpenSolid.Curve.Intersections qualified as Intersections
import {-# SOURCE #-} OpenSolid.Curve.Nondegenerate qualified as Curve.Nondegenerate
import {-# SOURCE #-} OpenSolid.Curve.Nonzero qualified as Curve.Nonzero
import OpenSolid.Curve.Segment (Segment)
import OpenSolid.Curve.Segment qualified as Curve.Segment
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.Desingularization.Curve qualified as Desingularization.Curve
import OpenSolid.DirectionBounds (DirectionBounds)
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Error (IsDegenerate (IsDegenerate))
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Line (Line (Line))
import OpenSolid.Line qualified as Line
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Number qualified as Number
import OpenSolid.Pair qualified as Pair
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point (Point)
import OpenSolid.Point qualified as Point
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Polyline (Polyline (Polyline))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Resolution (Resolution)
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Search qualified as Search
import OpenSolid.SearchDomain qualified as SearchDomain
import OpenSolid.SearchTree qualified as SearchTree
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import {-# SOURCE #-} OpenSolid.SurfaceFunction2D (SurfaceFunction2D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction2D qualified as SurfaceFunction2D
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.VectorBounds (VectorBounds)
import OpenSolid.VectorBounds qualified as VectorBounds
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve qualified as VectorCurve
import OpenSolid.VectorCurve.Nondegenerate qualified as VectorCurve.Nondegenerate
import OpenSolid.VectorCurve2D (VectorCurve2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D
import OpenSolid.VectorCurve3D (VectorCurve3D)
import OpenSolid.VectorCurve3D qualified as VectorCurve3D
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)
import OpenSolid.VectorSurfaceFunction3D qualified as VectorSurfaceFunction3D

data Curve dimension units space = Curve
  { compiled :: Compiled dimension units space
  , derivative :: ~(VectorCurve dimension units space)
  , startPoint :: ~(Point dimension units space)
  , endPoint :: ~(Point dimension units space)
  , searchTree :: ~(SearchTree dimension units space)
  }

-- | A parametric curve in 2D space.
type Curve2D units = Curve 2 units Void

-- | A parametric curve in 3D space.
type Curve3D space = Curve 3 Meters space

type Compiled dimension units space =
  CompiledFunction
    Number
    (Point dimension units space)
    (Interval Unitless)
    (Bounds dimension units space)

data HasSingularity = HasSingularity deriving (Eq, Show)

type SearchTree dimension units space =
  SearchTree.SearchTree (Interval Unitless) (Segment dimension units space)

instance Units.Coercion (Curve2D units1) (Curve2D units2) where
  coerce curve =
    Curve
      { compiled = Units.coerce curve.compiled
      , derivative = Units.coerce curve.derivative
      , startPoint = Units.coerce curve.startPoint
      , endPoint = Units.coerce curve.endPoint
      , searchTree = Units.coerce curve.searchTree
      }

instance FFI (Curve2D Meters) where
  representation = FFI.classRepresentation "Curve2D"

instance FFI (Curve2D Unitless) where
  representation = FFI.classRepresentation "UvCurve"

instance HasUnits (Curve dimension units space) units

instance
  Exists dimension units space =>
  ApproximateEquality (Curve dimension units space) (Tolerance units)
  where
  curve1 ~= curve2 = testPoints curve1 ~= testPoints curve2

instance
  (Exists dimension1 units1 space1, dimension1 ~ dimension2, space1 ~ space2, units1 ~ units2) =>
  Addition
    (Curve dimension1 units1 space1)
    (VectorCurve dimension2 units2 space2)
    (Curve dimension1 units1 space1)
  where
  lhs + rhs =
    new
      (compiled lhs + VectorCurve.compiled rhs)
      (derivative lhs + VectorCurve.derivative rhs)

instance
  (Exists dimension1 units1 space1, dimension1 ~ dimension2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve dimension1 units1 space1)
    (VectorCurve dimension2 units2 space2)
    (Curve dimension1 units1 space1)
  where
  lhs - rhs =
    new
      (compiled lhs - VectorCurve.compiled rhs)
      (derivative lhs - VectorCurve.derivative rhs)

instance
  (Exists dimension1 units1 space1, dimension1 ~ dimension2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve dimension1 units1 space1)
    (Curve dimension2 units2 space2)
    (VectorCurve dimension1 units1 space1)
  where
  lhs - rhs =
    VectorCurve.new (compiled lhs - compiled rhs) (derivative lhs - derivative rhs)

instance
  units1 ~ units2 =>
  Addition (Curve2D units1) (Vector2D units2) (Curve2D units1)
  where
  lhs + rhs = lhs + VectorCurve2D.constant rhs

instance
  units1 ~ units2 =>
  Subtraction (Curve2D units1) (Vector2D units2) (Curve2D units1)
  where
  lhs - rhs = lhs - VectorCurve2D.constant rhs

instance
  units1 ~ units2 =>
  Subtraction (Curve2D units1) (Point2D units2) (VectorCurve2D units1)
  where
  curve - givenPoint = curve - constant givenPoint

instance
  units1 ~ units2 =>
  Subtraction (Point2D units1) (Curve2D units2) (VectorCurve2D units1)
  where
  givenPoint - curve = constant givenPoint - curve

instance Composition (Curve2D units) (SurfaceFunction1D Unitless) (SurfaceFunction2D units) where
  curve . function =
    SurfaceFunction2D.new
      (compiled curve . function.compiled)
      (\p -> derivative curve . function * SurfaceFunction1D.derivative p function)

instance Composition (Curve2D units) SurfaceParameter (SurfaceFunction2D units) where
  curve . parameter = curve . SurfaceFunction1D.parameter parameter

instance Composition (SurfaceFunction1D units) (Curve2D Unitless) (Curve1D units) where
  f . g = do
    let (dudt, dvdt) = VectorCurve2D.components (derivative g)
    Curve1D.new (f.compiled . compiled g) (f.du . g * dudt + f.dv . g * dvdt)

instance
  Composition
    (VectorSurfaceFunction3D units space)
    (Curve2D Unitless)
    (VectorCurve3D units space)
  where
  function . uvCurve = do
    let (dudt, dvdt) = VectorCurve2D.components (derivative uvCurve)
    let compiledComposed = VectorSurfaceFunction3D.compiled function . compiled uvCurve
    let composedDerivative =
          VectorSurfaceFunction3D.derivative U function . uvCurve * dudt
            + VectorSurfaceFunction3D.derivative V function . uvCurve * dvdt
    VectorCurve3D.new compiledComposed composedDerivative

instance Composition (SurfaceFunction3D space) (Curve2D Unitless) (Curve3D space) where
  function . uvCurve = do
    let (dudt, dvdt) = VectorCurve2D.components (derivative uvCurve)
    new
      (function.compiled . compiled uvCurve)
      (function.du . uvCurve * dudt + function.dv . uvCurve * dvdt)

instance
  units1 ~ units2 =>
  Intersects (Curve2D units1) (Point2D units2) (Tolerance units1)
  where
  curve `intersects` givenPoint = not (List.isEmpty (findPoint givenPoint curve))

instance
  units1 ~ units2 =>
  Intersects (Point2D units1) (Curve2D units2) (Tolerance units1)
  where
  givenPoint `intersects` curve = curve `intersects` givenPoint

instance
  space1 ~ space2 =>
  Intersects (Curve3D space1) (Point3D space2) (Tolerance Meters)
  where
  curve `intersects` givenPoint = not (List.isEmpty (findPoint givenPoint curve))

instance
  space1 ~ space2 =>
  Intersects (Point3D space1) (Curve3D space2) (Tolerance Meters)
  where
  givenPoint `intersects` curve = curve `intersects` givenPoint

instance
  Exists dimension units space =>
  Composition (Curve dimension units space) (Curve1D Unitless) (Curve dimension units space)
  where
  f . g = new (f.compiled . Curve1D.compiled g) ((f.derivative . g) * Curve1D.derivative g)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition (Curve3D space1) (Vector3D meters space2) (Curve3D space1)
  where
  lhs + rhs = lhs + VectorCurve3D.constant rhs

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction (Curve3D space1) (Vector3D meters space2) (Curve3D space1)
  where
  lhs - rhs = lhs - VectorCurve3D.constant rhs

instance
  space1 ~ space2 =>
  Subtraction (Curve3D space1) (Point3D space2) (VectorCurve3D Meters space1)
  where
  lhs - rhs = lhs - constant rhs

instance
  space1 ~ space2 =>
  Subtraction (Point3D space1) (Curve3D space2) (VectorCurve3D Meters space1)
  where
  lhs - rhs = constant lhs - rhs

instance Composition (Curve3D space) (SurfaceFunction1D Unitless) (SurfaceFunction3D space) where
  curve . function =
    SurfaceFunction3D.new
      (compiled curve . function.compiled)
      (\p -> (derivative curve . function) * SurfaceFunction1D.derivative p function)

class
  ( Point.Exists dimension units space
  , Bounds.Exists dimension units space
  , Vector.Exists dimension units space
  , Vector.Exists dimension (Unitless ?/? units) space
  , VectorBounds.Exists dimension units space
  , VectorBounds.Exists dimension (Unitless ?/? units) space
  , DirectionBounds.Exists dimension space
  , Axis.Exists dimension units space
  , Expression.Constant Number (Point dimension units space)
  , Expression.BezierCurve (Point dimension units space)
  , Expression.Evaluation
      Number
      (Point dimension units space)
      (Interval Unitless)
      (Bounds dimension units space)
  , Addition
      (Expression Number (Point dimension units space))
      (Expression Number (Vector dimension units space))
      (Expression Number (Point dimension units space))
  , Subtraction
      (Expression Number (Point dimension units space))
      (Expression Number (Vector dimension units space))
      (Expression Number (Point dimension units space))
  , Subtraction
      (Expression Number (Point dimension units space))
      (Expression Number (Point dimension units space))
      (Expression Number (Vector dimension units space))
  , VectorCurve.Exists dimension units space
  , VectorCurve.Exists dimension (Unitless ?/? units) space
  , DirectionCurve.Exists dimension space
  , Subtraction
      (Curve dimension units space)
      (Point dimension units space)
      (VectorCurve dimension units space)
  , Subtraction
      (Point dimension units space)
      (Curve dimension units space)
      (VectorCurve dimension units space)
  , Desingularization.Curve
      (Curve dimension units space)
      (Point dimension units space)
      (Vector dimension units space)
  ) =>
  Exists dimension units space

instance Desingularization.Curve (Curve2D units) (Point2D units) (Vector2D units) where
  value = point
  derivativeValue = derivativeValue
  secondDerivativeValue = secondDerivativeValue
  bezier = bezier
  desingularized = desingularized

instance Desingularization.Curve (Curve3D space) (Point3D space) (Vector3D Meters space) where
  value = point
  derivativeValue = derivativeValue
  secondDerivativeValue = secondDerivativeValue
  bezier = bezier
  desingularized = desingularized

instance Exists 2 units Void

instance Exists 3 Meters space

new ::
  Exists dimension units space =>
  Compiled dimension units space ->
  VectorCurve dimension units space ->
  Curve dimension units space
new givenCompiled givenDerivative =
  recursive \self ->
    Curve
      { compiled = givenCompiled
      , derivative = givenDerivative
      , startPoint = CompiledFunction.value givenCompiled 0.0
      , endPoint = CompiledFunction.value givenCompiled 1.0
      , searchTree = SearchTree.build (Curve.Segment.new self) SearchDomain.curve
      }

constant ::
  Exists dimension units space =>
  Point dimension units space -> Curve dimension units space
constant givenPoint = new (CompiledFunction.constant givenPoint) VectorCurve.zero

line ::
  Exists dimension units space =>
  Line dimension units space ->
  Curve dimension units space
line (Line p1 p2) = lineFrom p1 p2

lineFrom ::
  Exists dimension units space =>
  Point dimension units space ->
  Point dimension units space ->
  Curve dimension units space
lineFrom p1 p2 = bezier (NonEmpty.two p1 p2)

bezier ::
  Exists dimension units space =>
  NonEmpty (Point dimension units space) ->
  Curve dimension units space
bezier controlPoints = do
  let compiledBezier = CompiledFunction.concrete (Expression.bezierCurve controlPoints)
  let bezierDerivative = VectorCurve.bezier (Bezier.derivative controlPoints)
  new compiledBezier bezierDerivative

quadraticBezier ::
  Exists dimension units space =>
  Point dimension units space ->
  Point dimension units space ->
  Point dimension units space ->
  Curve dimension units space
quadraticBezier p1 p2 p3 = bezier (NonEmpty.three p1 p2 p3)

cubicBezier ::
  Exists dimension units space =>
  Point dimension units space ->
  Point dimension units space ->
  Point dimension units space ->
  Point dimension units space ->
  Curve dimension units space
cubicBezier p1 p2 p3 p4 = bezier (NonEmpty.four p1 p2 p3 p4)

hermite ::
  Exists dimension units space =>
  Point dimension units space ->
  List (Vector dimension units space) ->
  Point dimension units space ->
  List (Vector dimension units space) ->
  Curve dimension units space
hermite start startDerivatives end endDerivatives =
  bezier (Bezier.hermite start startDerivatives end endDerivatives)

derivative :: Curve dimension units space -> VectorCurve dimension units space
derivative = (.derivative)

compiled :: Curve dimension units space -> Compiled dimension units space
compiled = (.compiled)

testPoints ::
  Exists dimension units space =>
  Curve dimension units space ->
  NonEmpty (Point dimension units space)
testPoints curve = NonEmpty.map (point curve) Parameter.samples

secondDerivative ::
  Exists dimension units space =>
  Curve dimension units space ->
  VectorCurve dimension units space
secondDerivative = VectorCurve.derivative . derivative

isPoint :: (Exists dimension units space, Tolerance units) => Curve dimension units space -> Bool
isPoint curve = VectorCurve.isZero (derivative curve)

point :: Curve dimension units space -> Number -> Point dimension units space
point curve 0.0 = curve.startPoint
point curve 1.0 = curve.endPoint
point curve tValue = CompiledFunction.value curve.compiled tValue

startPoint :: Curve dimension units space -> Point dimension units space
startPoint = (.startPoint)

endPoint :: Curve dimension units space -> Point dimension units space
endPoint = (.endPoint)

endpoints ::
  Curve dimension units space ->
  (Point dimension units space, Point dimension units space)
endpoints curve = (startPoint curve, endPoint curve)

bounds :: Curve dimension units space -> Interval Unitless -> Bounds dimension units space
bounds curve tBounds = CompiledFunction.bounds curve.compiled tBounds

overallBounds :: Curve dimension units space -> Bounds dimension units space
overallBounds curve = bounds curve Interval.unit

searchTree :: Curve dimension units space -> SearchTree dimension units space
searchTree = (.searchTree)

singular0 :: Exists dimension units space => Curve dimension units space -> Bool
singular0 curve = VectorCurve.singular0 (derivative curve)

singular1 :: Exists dimension units space => Curve dimension units space -> Bool
singular1 curve = VectorCurve.singular1 (derivative curve)

isOnAxis ::
  (Exists dimension units space, Tolerance units) =>
  Axis dimension units space ->
  Curve dimension units space ->
  Bool
isOnAxis axis curve = NonEmpty.all (intersects axis) (testPoints curve)

nondegenerate ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Result IsDegenerate (Nondegenerate (Curve dimension units space))
nondegenerate curve =
  if VectorCurve.isZero (derivative curve) then Error IsDegenerate else Ok (Nondegenerate curve)

nonzero ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Result HasSingularity (Nonzero (Curve dimension units space))
nonzero curve =
  if derivativeValue curve 0.0 ~= Vector.zero || derivativeValue curve 1.0 ~= Vector.zero
    then Error HasSingularity
    else Ok (Nonzero curve)

tangentDirection ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Result IsDegenerate (DirectionCurve dimension space)
tangentDirection curve = VectorCurve.direction (derivative curve)

curvatureVector_ ::
  ( Exists dimension units space
  , VectorCurve.Exists dimension (Unitless ?/? units) space
  , Tolerance units
  ) =>
  Curve dimension units space ->
  Result HasSingularity (VectorCurve dimension (Unitless ?/? units) space)
curvatureVector_ curve = Result.map Curve.Nonzero.curvatureVector_ (nonzero curve)

derivativeValue ::
  Exists dimension units space =>
  Curve dimension units space ->
  Number ->
  Vector dimension units space
derivativeValue curve tValue = VectorCurve.value (derivative curve) tValue

derivativeBounds ::
  Exists dimension units space =>
  Curve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
derivativeBounds curve tBounds = VectorCurve.bounds (derivative curve) tBounds

secondDerivativeValue ::
  Exists dimension units space =>
  Curve dimension units space ->
  Number ->
  Vector dimension units space
secondDerivativeValue curve tValue = VectorCurve.value (secondDerivative curve) tValue

secondDerivativeBounds ::
  Exists dimension units space =>
  Curve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
secondDerivativeBounds curve tBounds = VectorCurve.bounds (secondDerivative curve) tBounds

tangentDirectionBounds ::
  Exists dimension units space =>
  Curve dimension units space ->
  Interval Unitless ->
  DirectionBounds dimension space
tangentDirectionBounds curve tBounds = VectorCurve.directionBounds (derivative curve) tBounds

reverse ::
  Exists dimension units space =>
  Curve dimension units space ->
  Curve dimension units space
reverse curve = curve . (1.0 - Curve1D.t)

distanceAlong ::
  Exists dimension units space =>
  Axis dimension units space ->
  Curve dimension units space ->
  Curve1D units
distanceAlong axis curve = (curve - Axis.originPoint axis) `dot` Axis.direction axis

desingularize ::
  Exists dimension units space =>
  Maybe (Point dimension units space, Vector dimension units space) ->
  Curve dimension units space ->
  Maybe (Point dimension units space, Vector dimension units space) ->
  Curve dimension units space
desingularize = Desingularization.curve

desingularized ::
  Exists dimension units space =>
  Curve dimension units space ->
  Curve dimension units space ->
  Curve dimension units space ->
  Curve dimension units space
desingularized start middle end = do
  let compiledDesingularized =
        CompiledFunction.desingularized
          (Curve1D.compiled Curve1D.t)
          start.compiled
          middle.compiled
          end.compiled
  let desingularizedDerivative =
        VectorCurve.desingularized start.derivative middle.derivative end.derivative
  new compiledDesingularized desingularizedDerivative

findPoint ::
  (Exists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Curve dimension units space ->
  List Number
findPoint givenPoint curve = do
  let evaluate tValue = (# point curve tValue - givenPoint, derivativeValue curve tValue #)
  let isSolution tValue = point curve tValue ~= givenPoint
  let isDegenerate tValue = derivativeValue curve tValue ~= Vector.zero
  let endpointSolutions = List.filter isSolution [0.0, 1.0]
  let solveMonotonic tBounds = do
        let tMid = Interval.midpoint tBounds
        let tSolution = NewtonRaphson.curve evaluate tMid
        if Search.isInterior tSolution tBounds && isSolution tSolution
          then Resolved (Just tSolution)
          else Unresolved
  let interiorSolution tBounds segment
        | not (givenPoint `intersects` Curve.Segment.bounds segment) = Resolved Nothing
        | otherwise = do
            let isMonotonic = Curve.Segment.monotonic segment
            let isSmall = SearchDomain.isSmall tBounds
            let endpointSolution = List.find (Number.includedIn tBounds) endpointSolutions
            let hasEndpointSolution = endpointSolution /= Nothing
            if
              | isMonotonic && hasEndpointSolution -> Resolved Nothing
              | isSmall, Just tValue <- endpointSolution, isDegenerate tValue -> Resolved Nothing
              | isMonotonic -> solveMonotonic tBounds
              | otherwise -> Unresolved
  let isDuplicate (tBounds1, _) (tBounds2, _) = SearchDomain.overlapping tBounds1 tBounds2
  let interiorSolutions =
        Search.exclusive interiorSolution isDuplicate (searchTree curve)
          & List.map Pair.second
  List.sort (endpointSolutions <> interiorSolutions)

intersections ::
  ( Exists dimension units space
  , NewtonRaphson.Surface dimension units space
  , Tolerance units
  ) =>
  Curve dimension units space ->
  Curve dimension units space ->
  Result IsDegenerate (Maybe Intersections)
intersections = Intersections.intersections

linearDeviation ::
  Exists dimension units space =>
  Curve dimension units space ->
  Interval Unitless ->
  Quantity units
linearDeviation curve (Interval t1 t2) = do
  let p1 = point curve t1
  let p2 = point curve t2
  let pMid = point curve (Number.midpoint t1 t2)
  let midError = Line.distanceTo pMid (Line p1 p2)
  max midError (leftRightError curve t1 t2 p1 p2)

toPolyline ::
  Exists dimension units space =>
  Resolution units ->
  Curve dimension units space ->
  Polyline dimension units space
toPolyline resolution curve =
  Polyline (NonEmpty.map (point curve) (linearize resolution curve))

linearize ::
  Exists dimension units space =>
  Resolution units ->
  Curve dimension units space ->
  NonEmpty Number
linearize resolution curve = do
  let collect (Interval t1 t2) p1 p2 accumulated = do
        let tMid = Number.midpoint t1 t2
        let pMid = point curve tMid
        let midError = Line.distanceTo pMid (Line p1 p2)
        let error = max midError (leftRightError curve t1 t2 p1 p2)
        let size = Point.distanceFrom p1 p2
        if Resolution.acceptable ("size" ::: size) ("error" ::: error) resolution
          then NonEmpty.push t1 accumulated
          else
            accumulated
              & collect (Interval tMid t2) pMid p2
              & collect (Interval t1 tMid) p1 pMid
  collect Interval.unit (startPoint curve) (endPoint curve) (NonEmpty.one 1.0)

leftRightError ::
  Exists dimension units space =>
  Curve dimension units space ->
  Number ->
  Number ->
  Point dimension units space ->
  Point dimension units space ->
  Quantity units
leftRightError curve t1 t2 p1 p2 = do
  let tWidth = t2 - t1
  let tMid = t1 + 0.5 * tWidth
  let tOffset = 0.5 * tWidth * Number.sqrt (3 / 7)
  let tLeft = tMid + tOffset
  let tRight = tMid - tOffset
  let leftError = Line.distanceTo (point curve tLeft) (Line p1 p2)
  let rightError = Line.distanceTo (point curve tRight) (Line p1 p2)
  max leftError rightError

arcLengthParameterizationFunction ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  (Number -> Number, Quantity units)
arcLengthParameterizationFunction curve =
  case nondegenerate curve of
    Error IsDegenerate -> (id, Quantity.zero)
    Ok nondegenerateCurve -> do
      let dsdt t = Vector.magnitude (derivativeValue curve t)
      let d2sdt2 t =
            secondDerivativeValue curve t
              `dot` Curve.Nondegenerate.tangentDirectionValue nondegenerateCurve t
      ArcLength.parameterization dsdt d2sdt2

arcLengthParameterization ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  (Curve1D Unitless, Quantity units)
arcLengthParameterization curve =
  case nondegenerate curve of
    Error IsDegenerate -> (Curve1D.t, Quantity.zero)
    Ok nondegenerateCurve -> do
      let (parameterizationFunction, length) = arcLengthParameterizationFunction curve
      let parameterizationBounds (Interval uLow uHigh) =
            Interval (parameterizationFunction uLow) (parameterizationFunction uHigh)
      let compiledParameterization =
            CompiledFunction.abstract parameterizationFunction parameterizationBounds
      let nondegenerateDerivative = Curve.Nondegenerate.derivative nondegenerateCurve
      let nondegenerateDerivativeMagnitude = VectorCurve.Nondegenerate.magnitude nondegenerateDerivative
      let parameterizationCurve = recursive \self -> do
            let dtdu = Curve1D.constant length / nondegenerateDerivativeMagnitude
            Curve1D.new compiledParameterization (dtdu . self)
      (parameterizationCurve, length)

parameterizeByArcLength ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  (Curve dimension units space, Quantity units)
parameterizeByArcLength curve = do
  let (parameterization, length) = arcLengthParameterization curve
  (curve . parameterization, length)
