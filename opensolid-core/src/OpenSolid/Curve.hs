{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve
  ( Curve
  , Curve2D
  , Curve3D
  , Exists
  , Solver (..)
  , Compiled
  , Segment
  , SearchTree
  , Tree (Tree)
  , HasDegeneracy (HasDegeneracy)
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
  , bounds
  , point
  , range
  , startPoint
  , endPoint
  , endpoints
  , secondDerivative
  , derivativeValue
  , derivativeRange
  , secondDerivativeValue
  , secondDerivativeRange
  , tangentDirectionRange
  , reverse
  , isPoint
  , hasDegenerateStart
  , hasDegenerateEnd
  , isOnAxis
  , nondegenerate
  , nonzero
  , tangentDirection
  , curvatureVector_
  , distanceAlong
  , desingularize
  , desingularized
  , findPoint
  , bisectionTree
  , searchTree
  , crossingSolver
  , tangentSolver
  , Intersections (IntersectionPoints, OverlappingSegments)
  , IntersectionPoint
  , intersections
  , linearDeviation
  , linearize
  , toPolyline
  , arcLengthParameterization
  , length
  , uniformParameterization
  , uniformParameterizationValue
  , uniformPoint
  , transformBy
  , placeOn
  )
where

import OpenSolid.ArcLength qualified as ArcLength
import OpenSolid.Axis (Axis)
import OpenSolid.Axis qualified as Axis
import OpenSolid.Bezier qualified as Bezier
import OpenSolid.Bisection (Tree (Tree))
import OpenSolid.Bisection qualified as Bisection
import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import {-# SOURCE #-} OpenSolid.Curve.CrossingSolver qualified as Curve.CrossingSolver
import {-# SOURCE #-} OpenSolid.Curve.IntersectionPoint (IntersectionPoint)
import {-# SOURCE #-} OpenSolid.Curve.Intersections (Intersections)
import {-# SOURCE #-} OpenSolid.Curve.Intersections qualified as Intersections
import {-# SOURCE #-} OpenSolid.Curve.Nondegenerate qualified as Curve.Nondegenerate
import {-# SOURCE #-} OpenSolid.Curve.Nondegenerate.Intersections qualified as Curve.Nondegenerate.Intersections
import {-# SOURCE #-} OpenSolid.Curve.Nonzero qualified as Curve.Nonzero
import OpenSolid.Curve.Segment (Segment)
import OpenSolid.Curve.Segment qualified as Curve.Segment
import {-# SOURCE #-} OpenSolid.Curve.TangentSolver2D qualified as Curve.TangentSolver2D
import {-# SOURCE #-} OpenSolid.Curve.TangentSolver3D qualified as Curve.TangentSolver3D
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
import OpenSolid.NewtonRaphson.Curve qualified as NewtonRaphson.Curve
import OpenSolid.NewtonRaphson.Surface qualified as NewtonRaphson.Surface
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Nondegenerate qualified as Nondegenerate
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Number qualified as Number
import OpenSolid.Pair qualified as Pair
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Point (Point)
import OpenSolid.Point qualified as Point
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Polyline (Polyline (Polyline))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Resolution (Resolution)
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.SearchDomain qualified as SearchDomain
import OpenSolid.SearchTree qualified as SearchTree
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import {-# SOURCE #-} OpenSolid.SurfaceFunction2D (SurfaceFunction2D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction2D qualified as SurfaceFunction2D
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Transform (Transform)
import OpenSolid.Transform qualified as Transform
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
  , bounds :: ~(Bounds dimension units space)
  , searchTree :: Nondegenerate.Field (SearchTree dimension units space)
  , bisectionTree :: Nondegenerate.Field (BisectionTree dimension units space)
  , arcLengthParameterization :: Nondegenerate.Field (Quantity units, Curve1D Unitless)
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

data HasDegeneracy = HasDegeneracy deriving (Eq, Show)

type SearchTree dimension units space =
  SearchTree.SearchTree (Interval Unitless) (Segment dimension units space)

type BisectionTree dimension units space =
  Bisection.Tree (Interval Unitless) (Segment dimension units space)

buildBisectionTree ::
  Exists dimension units space =>
  Interval Unitless ->
  Nondegenerate (Curve dimension units space) ->
  BisectionTree dimension units space
buildBisectionTree tRange curve = do
  let (tLeft, tRight) = Interval.bisect tRange
  let left = buildBisectionTree tLeft curve
  let right = buildBisectionTree tRight curve
  Tree tRange (Curve.Segment.new curve tRange) (NonEmpty.two left right)

instance Units.Coercion (Curve2D units1) (Curve2D units2) where
  coerce curve =
    Curve
      { compiled = Units.coerce curve.compiled
      , derivative = Units.coerce curve.derivative
      , startPoint = Units.coerce curve.startPoint
      , endPoint = Units.coerce curve.endPoint
      , bounds = Units.coerce curve.bounds
      , searchTree = Units.coerce curve.searchTree
      , bisectionTree = Units.coerce curve.bisectionTree
      , arcLengthParameterization =
          Nondegenerate.map (Pair.mapFirst Units.coerce) curve.arcLengthParameterization
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
    new (compiled lhs + VectorCurve.compiled rhs) (derivative lhs + VectorCurve.derivative rhs)

instance
  (Exists dimension1 units1 space1, dimension1 ~ dimension2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve dimension1 units1 space1)
    (VectorCurve dimension2 units2 space2)
    (Curve dimension1 units1 space1)
  where
  lhs - rhs =
    new (compiled lhs - VectorCurve.compiled rhs) (derivative lhs - VectorCurve.derivative rhs)

instance
  (Exists dimension1 units1 space1, dimension1 ~ dimension2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve dimension1 units1 space1)
    (Curve dimension2 units2 space2)
    (VectorCurve dimension1 units1 space1)
  where
  lhs - rhs = VectorCurve.new (compiled lhs - compiled rhs) (derivative lhs - derivative rhs)

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
      (compiled curve . SurfaceFunction1D.compiled function)
      (\p -> derivative curve . function * SurfaceFunction1D.derivative p function)

instance Composition (Curve2D units) SurfaceParameter (SurfaceFunction2D units) where
  curve . parameter = curve . SurfaceFunction1D.parameter parameter

instance Composition (SurfaceFunction1D units) (Curve2D Unitless) (Curve1D units) where
  f . g = do
    let (dudt, dvdt) = VectorCurve2D.components (derivative g)
    let compiledComposed = SurfaceFunction1D.compiled f . compiled g
    let composedDerivative =
          SurfaceFunction1D.derivative U f . g * dudt
            + SurfaceFunction1D.derivative V f . g * dvdt
    Curve1D.new compiledComposed composedDerivative

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
    let compiledComposed = SurfaceFunction3D.compiled function . compiled uvCurve
    let composedDerivative =
          SurfaceFunction3D.derivative U function . uvCurve * dudt
            + SurfaceFunction3D.derivative V function . uvCurve * dvdt
    new compiledComposed composedDerivative

instance
  units1 ~ units2 =>
  Intersects (Curve2D units1) (Point2D units2) (Tolerance units1)
  where
  intersects curve givenPoint = intersects givenPoint curve

instance
  units1 ~ units2 =>
  Intersects (Point2D units1) (Curve2D units2) (Tolerance units1)
  where
  intersects = intersectsPoint

instance
  space1 ~ space2 =>
  Intersects (Curve3D space1) (Point3D space2) (Tolerance Meters)
  where
  intersects curve givenPoint = intersects givenPoint curve

instance
  space1 ~ space2 =>
  Intersects (Point3D space1) (Curve3D space2) (Tolerance Meters)
  where
  intersects = intersectsPoint

intersectsPoint ::
  (Exists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Curve dimension units space ->
  Bool
intersectsPoint givenPoint curve = case nondegenerate curve of
  Error IsDegenerate -> givenPoint ~= startPoint curve
  Ok nondegenerateCurve ->
    Curve.Nondegenerate.findPoint givenPoint nondegenerateCurve
      & not . List.isEmpty

instance
  Exists dimension units space =>
  Composition (Curve dimension units space) (Curve1D Unitless) (Curve dimension units space)
  where
  f . g = new (compiled f . Curve1D.compiled g) ((derivative f . g) * Curve1D.derivative g)

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
      (compiled curve . SurfaceFunction1D.compiled function)
      (\p -> (derivative curve . function) * SurfaceFunction1D.derivative p function)

data Solver dimension units space where
  Solver ::
    { resolve ::
        (Exists dimension units space, Tolerance units) =>
        (Interval Unitless, Interval Unitless) ->
        (Segment dimension units space, Segment dimension units space) ->
        Fuzzy (Maybe tag)
    , solve ::
        (Exists dimension units space, Tolerance units) =>
        Nondegenerate (Curve dimension units space) ->
        Nondegenerate (Curve dimension units space) ->
        tag ->
        (Interval Unitless, Interval Unitless) ->
        (Segment dimension units space, Segment dimension units space) ->
        Fuzzy (Maybe (IntersectionPoint dimension units space))
    } ->
    Solver dimension units space

class
  ( Point.Exists dimension units space
  , Bounds.Exists dimension units space
  , Transform.Exists dimension units space
  , Vector.Exists dimension units space
  , Vector.Exists dimension (Unitless ?/? units) space
  , VectorBounds.Exists dimension units space
  , VectorBounds.Exists dimension (Unitless ?/? units) space
  , DirectionBounds.Exists dimension space
  , Axis.Exists dimension units space
  , Expression.Constant Number (Point dimension units space)
  , Expression.BezierCurve (Point dimension units space)
  , Expression.TransformBy
      (Transform dimension Transform.Affine units space)
      (Expression Number (Point dimension units space))
      (Expression Number (Point dimension units space))
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
  , Intersects (Curve dimension units space) (Point dimension units space) (Tolerance units)
  , Intersects (Point dimension units space) (Curve dimension units space) (Tolerance units)
  , NewtonRaphson.Curve.Solver dimension units space
  , NewtonRaphson.Surface.Solver dimension units space
  ) =>
  Exists dimension units space
  where
  tangentSolver :: Solver dimension units space

crossingSolver :: Solver dimension units space
crossingSolver = Curve.CrossingSolver.solver

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

instance Exists 2 units Void where
  tangentSolver = Curve.TangentSolver2D.solver

instance Exists 3 Meters space where
  tangentSolver = Curve.TangentSolver3D.solver

new ::
  Exists dimension units space =>
  Compiled dimension units space ->
  VectorCurve dimension units space ->
  Curve dimension units space
new givenCompiled givenDerivative =
  recursive \curve ->
    Curve
      { compiled = givenCompiled
      , derivative = givenDerivative
      , startPoint = CompiledFunction.value givenCompiled 0.0
      , endPoint = CompiledFunction.value givenCompiled 1.0
      , bounds = CompiledFunction.range givenCompiled Interval.unit
      , searchTree =
          curve & Nondegenerate.field do
            \nondegenerateCurve -> SearchTree.build (Curve.Segment.new nondegenerateCurve) SearchDomain.curve
      , bisectionTree = Nondegenerate.field (buildBisectionTree Interval.unit) curve
      , arcLengthParameterization = Nondegenerate.field buildArcLengthParameterization curve
      }

buildArcLengthParameterization ::
  Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  (Quantity units, Curve1D Unitless)
buildArcLengthParameterization curve = do
  let dsdt t = Vector.magnitude (derivativeValue (Nondegenerate.unwrap curve) t)
  let d2sdt2 t = do
        let tangent = Curve.Nondegenerate.tangentDirectionValue curve t
        secondDerivativeValue (Nondegenerate.unwrap curve) t `dot` tangent
  let (arcLength, parameterizationValue) = ArcLength.parameterization dsdt d2sdt2
  (arcLength, uniformParameterizationCurve curve arcLength parameterizationValue)

uniformParameterizationCurve ::
  Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Quantity units ->
  (Number -> Number) ->
  Curve1D Unitless
uniformParameterizationCurve curve curveLength parameterizationValue =
  recursive \self -> do
    let parameterizationRange (Interval rLow rHigh) =
          Interval (parameterizationValue rLow) (parameterizationValue rHigh)
    Curve1D.new
      (CompiledFunction.abstract parameterizationValue parameterizationRange)
      (uniformParameterizationDerivative curve curveLength self)

uniformParameterizationDerivative ::
  Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Quantity units ->
  Curve1D Unitless ->
  Curve1D Unitless
uniformParameterizationDerivative curve curveLength curveUniformParameterization = do
  let nondegenerateDerivative = Curve.Nondegenerate.derivative curve
  let derivativeMagnitude = VectorCurve.Nondegenerate.magnitude nondegenerateDerivative
  let dtdr = Curve1D.constant curveLength / derivativeMagnitude
  dtdr . curveUniformParameterization

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

{-# INLINE derivative #-}
derivative :: Curve dimension units space -> VectorCurve dimension units space
derivative = (.derivative)

{-# INLINE compiled #-}
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

{-# INLINE isPoint #-}
isPoint :: (Exists dimension units space, Tolerance units) => Curve dimension units space -> Bool
isPoint curve = VectorCurve.isZero (derivative curve)

point :: Curve dimension units space -> Number -> Point dimension units space
point curve 0.0 = startPoint curve
point curve 1.0 = endPoint curve
point curve tValue = CompiledFunction.value (compiled curve) tValue

startPoint :: Curve dimension units space -> Point dimension units space
startPoint = (.startPoint)

endPoint :: Curve dimension units space -> Point dimension units space
endPoint = (.endPoint)

endpoints ::
  Curve dimension units space ->
  (Point dimension units space, Point dimension units space)
endpoints curve = (startPoint curve, endPoint curve)

range :: Curve dimension units space -> Interval Unitless -> Bounds dimension units space
range curve tRange = CompiledFunction.range (compiled curve) tRange

bounds :: Curve dimension units space -> Bounds dimension units space
bounds = (.bounds)

bisectionTree :: Nondegenerate (Curve dimension units space) -> BisectionTree dimension units space
bisectionTree = Nondegenerate.get (.bisectionTree)

searchTree :: Nondegenerate (Curve dimension units space) -> SearchTree dimension units space
searchTree = Nondegenerate.get (.searchTree)

hasDegenerateStart :: Exists dimension units space => Curve dimension units space -> Bool
hasDegenerateStart curve = VectorCurve.hasDegenerateStart (derivative curve)

hasDegenerateEnd :: Exists dimension units space => Curve dimension units space -> Bool
hasDegenerateEnd curve = VectorCurve.hasDegenerateEnd (derivative curve)

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
  Result HasDegeneracy (Nonzero (Curve dimension units space))
nonzero curve =
  if derivativeValue curve 0.0 ~= Vector.zero || derivativeValue curve 1.0 ~= Vector.zero
    then Error HasDegeneracy
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
  Result HasDegeneracy (VectorCurve dimension (Unitless ?/? units) space)
curvatureVector_ curve = Result.map Curve.Nonzero.curvatureVector_ (nonzero curve)

derivativeValue ::
  Exists dimension units space =>
  Curve dimension units space ->
  Number ->
  Vector dimension units space
derivativeValue curve tValue = VectorCurve.value (derivative curve) tValue

derivativeRange ::
  Exists dimension units space =>
  Curve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
derivativeRange curve tRange = VectorCurve.range (derivative curve) tRange

secondDerivativeValue ::
  Exists dimension units space =>
  Curve dimension units space ->
  Number ->
  Vector dimension units space
secondDerivativeValue curve tValue = VectorCurve.value (secondDerivative curve) tValue

secondDerivativeRange ::
  Exists dimension units space =>
  Curve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
secondDerivativeRange curve tRange = VectorCurve.range (secondDerivative curve) tRange

tangentDirectionRange ::
  Exists dimension units space =>
  Curve dimension units space ->
  Interval Unitless ->
  DirectionBounds dimension space
tangentDirectionRange curve tRange = VectorCurve.directionRange (derivative curve) tRange

reverse ::
  Exists dimension units space =>
  Curve dimension units space ->
  Curve dimension units space
reverse curve =
  recursive \reversed ->
    Curve
      { compiled = compiled curve . Curve1D.compiled (1.0 - Curve1D.t)
      , derivative = negate (VectorCurve.reverse (derivative curve))
      , startPoint = curve.endPoint
      , endPoint = curve.startPoint
      , bounds = curve.bounds
      , -- TODO optimize by adding e.g. a Segment.reverse function,
        -- to be able to reuse the existing search tree
        searchTree =
          reversed & Nondegenerate.field do
            \nondegenerateReversed ->
              SearchTree.build (Curve.Segment.new nondegenerateReversed) SearchDomain.curve
      , bisectionTree = Nondegenerate.field (buildBisectionTree Interval.unit) reversed
      , arcLengthParameterization =
          curve.arcLengthParameterization
            & Nondegenerate.map (Pair.mapSecond Curve1D.reverse)
      }

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
          (compiled start)
          (compiled middle)
          (compiled end)
  let desingularizedDerivative =
        VectorCurve.desingularized start.derivative middle.derivative end.derivative
  new compiledDesingularized desingularizedDerivative

findPoint ::
  (Exists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Curve dimension units space ->
  Result IsDegenerate (List Number)
findPoint givenPoint curve =
  Result.map (Curve.Nondegenerate.findPoint givenPoint) (nondegenerate curve)

intersections ::
  ( Exists dimension units space
  , NewtonRaphson.Surface.Solver dimension units space
  , Tolerance units
  ) =>
  Curve dimension units space ->
  Curve dimension units space ->
  Result IsDegenerate (Maybe (Intersections dimension units space))
intersections curve1 curve2 = do
  nondegenerate1 <- nondegenerate curve1
  nondegenerate2 <- nondegenerate curve2
  Ok (Curve.Nondegenerate.Intersections.intersections nondegenerate1 nondegenerate2)

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

arcLengthParameterization ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  (Quantity units, Curve1D Unitless)
arcLengthParameterization curve =
  case nondegenerate curve of
    Ok nondegenerateCurve -> Nondegenerate.get (.arcLengthParameterization) nondegenerateCurve
    Error IsDegenerate -> (Quantity.zero, Curve1D.t)

length ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Quantity units
length = Pair.first . arcLengthParameterization

uniformParameterization ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Curve1D Unitless
uniformParameterization = Pair.second . arcLengthParameterization

uniformParameterizationValue ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Number ->
  Number
uniformParameterizationValue curve r = Curve1D.value (uniformParameterization curve) r

uniformPoint ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Number ->
  Point dimension units space
uniformPoint curve r = point curve (uniformParameterizationValue curve r)

transformBy ::
  Exists dimension units space =>
  Transform dimension tag units space ->
  Curve dimension units space ->
  Curve dimension units space
transformBy transform curve =
  recursive \transformed -> do
    let compiledTransformed =
          CompiledFunction.map
            (Expression.transformBy (Transform.asAffine transform))
            (Point.transformBy transform)
            (Bounds.transformBy transform)
            (compiled curve)
    let transformedDerivative =
          VectorCurve.transformBy (Transform.vectorTransform transform) (derivative curve)
    Curve
      { compiled = compiledTransformed
      , derivative = transformedDerivative
      , startPoint = Point.transformBy transform curve.startPoint
      , endPoint = Point.transformBy transform curve.endPoint
      , bounds = CompiledFunction.range compiledTransformed Interval.unit
      , -- TODO optimize by transforming existing search tree?
        searchTree =
          transformed & Nondegenerate.field do
            \nondegenerateTransformed ->
              SearchTree.build (Curve.Segment.new nondegenerateTransformed) SearchDomain.curve
      , bisectionTree = Nondegenerate.field (buildBisectionTree Interval.unit) transformed
      , arcLengthParameterization =
          case Transform.uniformScale transform of
            Just uniformScale ->
              curve.arcLengthParameterization
                & Nondegenerate.map (Pair.mapFirst (* Number.abs uniformScale))
            Nothing -> Nondegenerate.field buildArcLengthParameterization transformed
      }

placeOn :: Plane3D space -> Curve2D Meters -> Curve3D space
placeOn plane curve =
  recursive \placed -> do
    let compiledPlaced =
          CompiledFunction.map
            (Expression.placeOn plane)
            (Point2D.placeOn plane)
            (Bounds2D.placeOn plane)
            (compiled curve)
    let placedDerivative = VectorCurve3D.on plane (derivative curve)
    Curve
      { compiled = compiledPlaced
      , derivative = placedDerivative
      , startPoint = Point2D.placeOn plane curve.startPoint
      , endPoint = Point2D.placeOn plane curve.endPoint
      , bounds = CompiledFunction.range compiledPlaced Interval.unit
      , searchTree =
          placed & Nondegenerate.field do
            \nondegeneratePlaced ->
              SearchTree.build (Curve.Segment.new nondegeneratePlaced) SearchDomain.curve
      , bisectionTree = Nondegenerate.field (buildBisectionTree Interval.unit) placed
      , arcLengthParameterization = curve.arcLengthParameterization
      }
