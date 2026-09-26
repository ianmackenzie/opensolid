{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve
  ( Curve
  , Curve2D
  , Curve3D
  , UvCurve
  , CurveExists
  , Solver (..)
  , Compiled
  , Segment
  , BisectionTree
  , IsDegenerate (IsDegenerate)
  , HasDegeneracy (HasDegeneracy)
  , IsDegenerateAndCoincidentWithPoint (IsDegenerateAndCoincidentWithPoint)
  , new
  , unsafe
  , displacedFrom
  , line
  , lineFrom
  , bezier
  , quadraticBezier
  , cubicBezier
  , hermite
  , derivative
  , compiled
  , bounds
  , pointAt
  , pointOn
  , range
  , startPoint
  , endPoint
  , endpoints
  , secondDerivative
  , startDerivative
  , endDerivative
  , derivativeAt
  , derivativeRange
  , startSecondDerivative
  , endSecondDerivative
  , secondDerivativeAt
  , secondDerivativeRange
  , tangentDirectionAt
  , tangentDirectionRange
  , curvatureAt_
  , curvatureVectorAt_
  , curvatureRange_
  , curvatureVectorRange_
  , reverse
  , isPoint
  , hasDegenerateStart
  , hasDegenerateEnd
  , isOnAxis
  , nonzero
  , distanceAlong
  , desingularizeStart
  , desingularizeEnd
  , findPoint
  , continuityAt
  , bisectionTree
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
  , fromUniform
  , atUniform
  , transformBy
  , orthonormalTransform
  , convert
  , placeOn
  , displaceBy
  )
where

import OpenSolid.ArcLength qualified as ArcLength
import OpenSolid.Axis (Axis, AxisExists)
import OpenSolid.Axis qualified as Axis
import OpenSolid.Bag qualified as Bag
import OpenSolid.Bezier qualified as Bezier
import OpenSolid.Bisection qualified as Bisection
import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import OpenSolid.Bounds (Bounds, BoundsExists)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Continuity (Continuity)
import OpenSolid.Continuity qualified as Continuity
import {-# SOURCE #-} OpenSolid.Curve.CrossingSolver qualified as Curve.CrossingSolver
import OpenSolid.Curve.IntersectionPoint (IntersectionPoint)
import {-# SOURCE #-} OpenSolid.Curve.Intersections (Intersections)
import {-# SOURCE #-} OpenSolid.Curve.Intersections qualified as Curve.Intersections
import OpenSolid.Curve.Segment (Segment (..))
import OpenSolid.Curve.Segment qualified as Curve.Segment
import {-# SOURCE #-} OpenSolid.Curve.TangentSolver2D qualified as Curve.TangentSolver2D
import {-# SOURCE #-} OpenSolid.Curve.TangentSolver3D qualified as Curve.TangentSolver3D
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Degeneracy qualified as Degeneracy
import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.DirectionBounds (DirectionBounds, DirectionBoundsExists)
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Fuzzy qualified as Fuzzy
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.IsZero (IsZero (IsZero))
import OpenSolid.Line (Line (Line))
import OpenSolid.Line qualified as Line
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson.Curve qualified as NewtonRaphson.Curve
import OpenSolid.NewtonRaphson.Surface qualified as NewtonRaphson.Surface
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Number qualified as Number
import OpenSolid.Pair qualified as Pair
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Point (Point, PointExists)
import OpenSolid.Point qualified as Point
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Polyline (Polyline (Polyline))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Resolution (Resolution)
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Show qualified as Show
import OpenSolid.Space qualified as Space
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import {-# SOURCE #-} OpenSolid.SurfaceFunction2D (SurfaceFunction2D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction2D qualified as SurfaceFunction2D
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform (Transform, TransformExists)
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform.Tag qualified as Transform.Tag
import OpenSolid.Units (Units)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector (Vector, VectorExists)
import OpenSolid.Vector qualified as Vector
import OpenSolid.Vector.Nonzero qualified as Vector.Nonzero
import OpenSolid.VectorBounds (VectorBounds, VectorBoundsExists)
import OpenSolid.VectorBounds qualified as VectorBounds
import OpenSolid.VectorCurve (VectorCurve, VectorCurveExists)
import OpenSolid.VectorCurve qualified as VectorCurve
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
  , hasDegenerateStart :: ~Bool
  , hasDegenerateEnd :: ~Bool
  , bisectionTree :: ~(BisectionTree dimension units space)
  , arcLengthParameterization :: ~(Quantity units, Number -> Number)
  }

-- | A parametric curve in 2D space.
type Curve2D units = Curve 2 units Void

type UvCurve = Curve 2 Unitless Void

instance units1 ~ units2 => Bounded (Curve2D units1) (Bounds2D units2) where
  {-# INLINE bounds #-}
  bounds = bounds

instance Show (Curve2D units) where
  showsPrec precedence curve =
    Show.partialRecord precedence "Curve2D" $
      [ ("startPoint", Text.show curve.startPoint)
      , ("endPoint", Text.show curve.endPoint)
      ]

-- | A parametric curve in 3D space.
type Curve3D space = Curve 3 Meters space

instance space1 ~ space2 => Bounded (Curve3D space1) (Bounds3D space2) where
  {-# INLINE bounds #-}
  bounds = bounds

instance Show (Curve3D space) where
  showsPrec precedence curve =
    Show.partialRecord precedence "Curve3D" $
      [ ("startPoint", Text.show curve.startPoint)
      , ("endPoint", Text.show curve.endPoint)
      ]

type Compiled dimension units space =
  CompiledFunction
    Number
    (Point dimension units space)
    (Interval Unitless)
    (Bounds dimension units space)

data IsDegenerate dimension units space
  = IsDegenerate (Point dimension units space)

deriving instance PointExists dimension units space => Eq (IsDegenerate dimension units space)

deriving instance PointExists dimension units space => Show (IsDegenerate dimension units space)

deriving instance PointExists dimension units space => Err (IsDegenerate dimension units space)

data HasDegeneracy = HasDegeneracy deriving (Eq, Show, Err)

data IsDegenerateAndCoincidentWithPoint
  = IsDegenerateAndCoincidentWithPoint
  deriving (Eq, Show, Err)

type BisectionTree dimension units space =
  Bisection.Tree (Interval Unitless) (Segment dimension units space)

buildBisectionTree ::
  CurveExists dimension units space =>
  Interval Unitless ->
  Curve dimension units space ->
  BisectionTree dimension units space
buildBisectionTree tRange curve = do
  let (tLeft, tRight) = Interval.bisect tRange
  let left = buildBisectionTree tLeft curve
  let right = buildBisectionTree tRight curve
  Bisection.Tree tRange (subsegment curve tRange) (NonEmpty.two left right)

subsegment ::
  CurveExists dimension units space =>
  Curve dimension units space ->
  Interval Unitless ->
  Segment dimension units space
subsegment curve tRange = do
  let Interval t1 t2 = tRange
  let p1 = pointAt t1 curve
  let p2 = pointAt t2 curve
  let segmentRange0 = range tRange curve
  let segmentDerivativeRange = derivativeRange tRange curve
  let segmentSecondDerivativeRange = secondDerivativeRange tRange curve
  let halfDisplacementRange = 0.5 * Interval.width tRange * segmentDerivativeRange
  let leftRange = Bounds.aggregate2 (Bounds.constant p1) (p1 + halfDisplacementRange)
  let rightRange = Bounds.aggregate2 (Bounds.constant p2) (p2 - halfDisplacementRange)
  let segmentRange1 = Bounds.aggregate2 leftRange rightRange
  let segmentRange =
        case Bounds.intersection segmentRange0 segmentRange1 of
          Just intersection -> intersection
          Nothing -> error "Curve bounds and derivative bounds are inconsistent"
  let segmentTangentDirectionRange =
        tangentDirectionRangeImpl tRange curve segmentDerivativeRange segmentSecondDerivativeRange
  let (segmentCurvatureMagnitudeRange_, segmentCurvatureDirectionRange) =
        curvatureRangeImpl_
          segmentDerivativeRange
          segmentSecondDerivativeRange
          segmentTangentDirectionRange
  let segmentCurvatureVectorRange_ =
        segmentCurvatureMagnitudeRange_ * segmentCurvatureDirectionRange
  let isDegenerateStart = t2 <= Degeneracy.tStart && hasDegenerateStart curve
  let isDegenerateEnd = t1 >= Degeneracy.tEnd && hasDegenerateEnd curve
  Segment
    { range = segmentRange
    , derivativeRange = segmentDerivativeRange
    , secondDerivativeRange = segmentSecondDerivativeRange
    , tangentDirectionRange = segmentTangentDirectionRange
    , curvatureVectorRange_ = segmentCurvatureVectorRange_
    , isDegenerate = isDegenerateStart || isDegenerateEnd
    }

instance Units.Coercion (Curve2D units1) (Curve2D units2) where
  coerce curve =
    Curve
      { compiled = Units.coerce curve.compiled
      , derivative = Units.coerce curve.derivative
      , startPoint = Units.coerce curve.startPoint
      , endPoint = Units.coerce curve.endPoint
      , bounds = Units.coerce curve.bounds
      , hasDegenerateStart = curve.hasDegenerateStart
      , hasDegenerateEnd = curve.hasDegenerateEnd
      , bisectionTree = Bisection.map Units.coerce curve.bisectionTree
      , arcLengthParameterization = Pair.mapFirst Units.coerce curve.arcLengthParameterization
      }

instance Space.Coercion (Curve3D space1) (Curve3D space2) where
  coerce curve =
    Curve
      { compiled = Space.coerce curve.compiled
      , derivative = Space.coerce curve.derivative
      , startPoint = Space.coerce curve.startPoint
      , endPoint = Space.coerce curve.endPoint
      , bounds = Space.coerce curve.bounds
      , hasDegenerateStart = curve.hasDegenerateStart
      , hasDegenerateEnd = curve.hasDegenerateEnd
      , bisectionTree = Bisection.map Space.coerce curve.bisectionTree
      , arcLengthParameterization = curve.arcLengthParameterization
      }

instance FFI (Curve2D Meters) where
  representation = FFI.classRepresentation "Curve2D"

instance FFI UvCurve where
  representation = FFI.classRepresentation "UvCurve"

instance Units (Curve dimension units space) units

instance
  CurveExists dimension units space =>
  ApproximateEquality (Curve dimension units space) units
  where
  curve1 ~= curve2 = testPoints curve1 ~= testPoints curve2

instance
  ( CurveExists dimension1 units1 space1
  , dimension1 ~ dimension2
  , space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Curve dimension1 units1 space1)
    (Curve dimension2 units2 space2)
    (VectorCurve dimension1 units1 space1)
  where
  lhs - rhs = VectorCurve.new (compiled lhs - compiled rhs) (derivative lhs - derivative rhs)

instance
  units1 ~ units2 =>
  Subtraction (Curve2D units1) (Point2D units2) (VectorCurve2D units1)
  where
  curve - givenPoint =
    VectorCurve.new
      (compiled curve - CompiledFunction.constant givenPoint)
      (derivative curve)

instance
  units1 ~ units2 =>
  Subtraction (Point2D units1) (Curve2D units2) (VectorCurve2D units1)
  where
  givenPoint - curve =
    VectorCurve.new
      (CompiledFunction.constant givenPoint - compiled curve)
      (negate (derivative curve))

instance Composition () (Curve2D units) (SurfaceFunction1D Unitless) (SurfaceFunction2D units) where
  f << g = do
    let dfdt = derivative f << g
    let (dtdu, dtdv) = SurfaceFunction1D.partialDerivatives g
    let compiledComposed = compiled f << SurfaceFunction1D.compiled g
    let composedPartialDerivatives = (dfdt * dtdu, dfdt * dtdv)
    SurfaceFunction2D.new compiledComposed composedPartialDerivatives

instance Composition () (SurfaceFunction1D units) UvCurve (Curve1D units) where
  f << g = do
    let (dfdu, dfdv) = Pair.map (<< g) (SurfaceFunction1D.partialDerivatives f)
    let (dudt, dvdt) = VectorCurve2D.components (derivative g)
    let compiledComposed = SurfaceFunction1D.compiled f << compiled g
    let composedDerivative = dfdu * dudt + dfdv * dvdt
    Curve1D.new compiledComposed composedDerivative

instance
  Composition
    ()
    (VectorSurfaceFunction3D units space)
    UvCurve
    (VectorCurve3D units space)
  where
  f << g = do
    let (dfdu, dfdv) = Pair.map (<< g) (VectorSurfaceFunction3D.partialDerivatives f)
    let (dudt, dvdt) = VectorCurve2D.components (derivative g)
    let compiledComposed = VectorSurfaceFunction3D.compiled f << compiled g
    let composedDerivative = dfdu * dudt + dfdv * dvdt
    VectorCurve3D.new compiledComposed composedDerivative

instance
  Composition
    (Tolerance Meters)
    (SurfaceFunction3D space)
    UvCurve
    (Result (IsDegenerate 3 Meters space) (Curve3D space))
  where
  f << g = do
    let (dfdu, dfdv) = Pair.map (<< g) (SurfaceFunction3D.partialDerivatives f)
    let (dudt, dvdt) = VectorCurve2D.components (derivative g)
    let compiledComposed = SurfaceFunction3D.compiled f << compiled g
    let composedDerivative = dfdu * dudt + dfdv * dvdt
    new compiledComposed composedDerivative

instance
  units1 ~ units2 =>
  Intersects (Curve2D units1) (Point2D units2) units1
  where
  curve ^ givenPoint = givenPoint ^ curve

instance
  units1 ~ units2 =>
  Intersects (Point2D units1) (Curve2D units2) units1
  where
  (^) = intersectsPoint

instance
  space1 ~ space2 =>
  Intersects (Curve3D space1) (Point3D space2) Meters
  where
  curve ^ givenPoint = givenPoint ^ curve

instance
  space1 ~ space2 =>
  Intersects (Point3D space1) (Curve3D space2) Meters
  where
  (^) = intersectsPoint

intersectsPoint ::
  (CurveExists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Curve dimension units space ->
  Bool
intersectsPoint givenPoint curve =
  not (List.isEmpty (findPoint givenPoint curve))

instance
  CurveExists dimension units space =>
  Composition
    (Tolerance units)
    (Curve dimension units space)
    (Curve1D Unitless)
    (Result (IsDegenerate dimension units space) (Curve dimension units space))
  where
  f << g = new (compiled f << Curve1D.compiled g) ((derivative f << g) * Curve1D.derivative g)

instance
  space1 ~ space2 =>
  Subtraction (Curve3D space1) (Point3D space2) (VectorCurve3D Meters space1)
  where
  curve - givenPoint =
    VectorCurve.new
      (compiled curve - CompiledFunction.constant givenPoint)
      (derivative curve)

instance
  space1 ~ space2 =>
  Subtraction (Point3D space1) (Curve3D space2) (VectorCurve3D Meters space1)
  where
  givenPoint - curve =
    VectorCurve.new
      (CompiledFunction.constant givenPoint - compiled curve)
      (negate (derivative curve))

instance Composition () (Curve3D space) (SurfaceFunction1D Unitless) (SurfaceFunction3D space) where
  f << g = do
    let dfdt = derivative f << g
    let (dtdu, dtdv) = SurfaceFunction1D.partialDerivatives g
    let compiledComposed = compiled f << SurfaceFunction1D.compiled g
    let composedPartialDerivatives = (dfdt * dtdu, dfdt * dtdv)
    SurfaceFunction3D.new compiledComposed composedPartialDerivatives

data Solver dimension units space where
  Solver ::
    { resolve ::
        (CurveExists dimension units space, Tolerance units) =>
        (Interval Unitless, Interval Unitless) ->
        (Segment dimension units space, Segment dimension units space) ->
        Fuzzy (Maybe tag)
    , solve ::
        (CurveExists dimension units space, Tolerance units) =>
        Curve dimension units space ->
        Curve dimension units space ->
        tag ->
        (Interval Unitless, Interval Unitless) ->
        (Segment dimension units space, Segment dimension units space) ->
        Fuzzy (Maybe IntersectionPoint)
    } ->
    Solver dimension units space

class
  ( PointExists dimension units space
  , BoundsExists dimension units space
  , TransformExists dimension units space
  , VectorExists dimension units space
  , VectorExists dimension (Unitless ?/? units) space
  , VectorBoundsExists dimension units space
  , VectorBoundsExists dimension (Unitless ?/? units) space
  , DirectionBoundsExists dimension space
  , AxisExists dimension units space
  , Expression.Constant Number (Point dimension units space)
  , Expression.BezierCurve (Point dimension units space)
  , Expression.TransformBy
      (Transform dimension Transform.Tag.Orthonormal units space)
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
      (Expression Number (Point dimension units space))
      (Expression Number (Vector dimension units space))
  , VectorCurveExists dimension units space
  , VectorCurveExists dimension (Unitless ?/? units) space
  , Subtraction
      (Curve dimension units space)
      (Point dimension units space)
      (VectorCurve dimension units space)
  , Subtraction
      (Point dimension units space)
      (Curve dimension units space)
      (VectorCurve dimension units space)
  , Intersects (Curve dimension units space) (Point dimension units space) units
  , Intersects (Point dimension units space) (Curve dimension units space) units
  , NewtonRaphson.Curve.Solver dimension units space
  , NewtonRaphson.Surface.Solver dimension units space
  ) =>
  CurveExists dimension units space
  where
  tangentSolver :: Solver dimension units space

crossingSolver :: Solver dimension units space
crossingSolver = Curve.CrossingSolver.solver

instance CurveExists 2 units Void where
  tangentSolver = Curve.TangentSolver2D.solver

instance CurveExists 3 Meters space where
  tangentSolver = Curve.TangentSolver3D.solver

new ::
  (CurveExists dimension units space, Tolerance units) =>
  Compiled dimension units space ->
  VectorCurve dimension units space ->
  Result (IsDegenerate dimension units space) (Curve dimension units space)
new givenCompiled givenDerivative = do
  let derivativeMagnitude tValue = Vector.magnitude (VectorCurve.valueAt tValue givenDerivative)
  let maxDerivativeMagnitude = NonEmpty.maximumOf derivativeMagnitude Parameter.samples
  if maxDerivativeMagnitude ~= Quantity.zero
    then Err (IsDegenerate (CompiledFunction.value 0.0 givenCompiled))
    else Ok (unsafe givenCompiled givenDerivative)

unsafe ::
  (CurveExists dimension units space, Tolerance units) =>
  Compiled dimension units space ->
  VectorCurve dimension units space ->
  Curve dimension units space
unsafe givenCompiled givenDerivative =
  recursive \curve ->
    Curve
      { compiled = givenCompiled
      , derivative = givenDerivative
      , startPoint = CompiledFunction.value 0.0 givenCompiled
      , endPoint = CompiledFunction.value 1.0 givenCompiled
      , bounds = CompiledFunction.range Interval.unit givenCompiled
      , hasDegenerateStart = VectorCurve.startValue givenDerivative ~= Vector.zero
      , hasDegenerateEnd = VectorCurve.endValue givenDerivative ~= Vector.zero
      , bisectionTree = buildBisectionTree Interval.unit curve
      , arcLengthParameterization = buildArcLengthParameterization curve
      }

buildArcLengthParameterization ::
  CurveExists dimension units space =>
  Curve dimension units space ->
  (Quantity units, Number -> Number)
buildArcLengthParameterization curve = do
  let dsdt tValue = Vector.magnitude (derivativeAt tValue curve)
  let d2sdt2 tValue = secondDerivativeAt tValue curve `dot` tangentDirectionAt tValue curve
  ArcLength.parameterization dsdt d2sdt2

displacedFrom ::
  (CurveExists dimension units space, Tolerance units) =>
  Point dimension units space ->
  VectorCurve dimension units space ->
  Result (IsDegenerate dimension units space) (Curve dimension units space)
displacedFrom point displacementCurve =
  new
    (CompiledFunction.constant point + VectorCurve.compiled displacementCurve)
    (VectorCurve.derivative displacementCurve)

line ::
  (CurveExists dimension units space, Tolerance units) =>
  Line dimension units space ->
  Result (IsDegenerate dimension units space) (Curve dimension units space)
line (Line p1 p2) = lineFrom p1 p2

lineFrom ::
  (CurveExists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Point dimension units space ->
  Result (IsDegenerate dimension units space) (Curve dimension units space)
lineFrom p1 p2 = bezier (NonEmpty.two p1 p2)

bezier ::
  (CurveExists dimension units space, Tolerance units) =>
  NonEmpty (Point dimension units space) ->
  Result (IsDegenerate dimension units space) (Curve dimension units space)
bezier controlPoints = do
  let compiledBezier = CompiledFunction.concrete (Expression.bezierCurve controlPoints)
  let bezierDerivative = VectorCurve.bezier (Bezier.derivative controlPoints)
  new compiledBezier bezierDerivative

quadraticBezier ::
  (CurveExists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Point dimension units space ->
  Point dimension units space ->
  Result (IsDegenerate dimension units space) (Curve dimension units space)
quadraticBezier p1 p2 p3 = bezier (NonEmpty.three p1 p2 p3)

cubicBezier ::
  (CurveExists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Point dimension units space ->
  Point dimension units space ->
  Point dimension units space ->
  Result (IsDegenerate dimension units space) (Curve dimension units space)
cubicBezier p1 p2 p3 p4 = bezier (NonEmpty.four p1 p2 p3 p4)

hermite ::
  (CurveExists dimension units space, Tolerance units) =>
  Point dimension units space ->
  List (Vector dimension units space) ->
  Point dimension units space ->
  List (Vector dimension units space) ->
  Result (IsDegenerate dimension units space) (Curve dimension units space)
hermite start startDerivatives end endDerivatives =
  bezier (Bezier.hermite start startDerivatives end endDerivatives)

{-# INLINE derivative #-}
derivative :: Curve dimension units space -> VectorCurve dimension units space
derivative = (.derivative)

{-# INLINE compiled #-}
compiled :: Curve dimension units space -> Compiled dimension units space
compiled = (.compiled)

testPoints ::
  CurveExists dimension units space =>
  Curve dimension units space ->
  NonEmpty (Point dimension units space)
testPoints curve = NonEmpty.map (pointOn curve) Parameter.samples

secondDerivative ::
  CurveExists dimension units space =>
  Curve dimension units space ->
  VectorCurve dimension units space
secondDerivative = VectorCurve.derivative . derivative

{-# INLINE isPoint #-}
isPoint ::
  (CurveExists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Bool
isPoint curve = VectorCurve.isZero (derivative curve)

pointAt :: Number -> Curve dimension units space -> Point dimension units space
pointAt 0.0 curve = startPoint curve
pointAt 1.0 curve = endPoint curve
pointAt tValue curve = CompiledFunction.value tValue (compiled curve)

pointOn :: Curve dimension units space -> Number -> Point dimension units space
pointOn curve tValue = pointAt tValue curve

startPoint :: Curve dimension units space -> Point dimension units space
startPoint = (.startPoint)

endPoint :: Curve dimension units space -> Point dimension units space
endPoint = (.endPoint)

endpoints ::
  Curve dimension units space ->
  (Point dimension units space, Point dimension units space)
endpoints curve = (startPoint curve, endPoint curve)

range :: Interval Unitless -> Curve dimension units space -> Bounds dimension units space
range tRange curve = CompiledFunction.range tRange (compiled curve)

bounds :: Curve dimension units space -> Bounds dimension units space
bounds = (.bounds)

tangentDirectionAt ::
  CurveExists dimension units space =>
  Number ->
  Curve dimension units space ->
  Direction dimension space
tangentDirectionAt tValue curve = do
  let firstDerivativeValue = derivativeAt tValue curve
  let secondDerivativeValue = secondDerivativeAt tValue curve
  tangentDirectionImpl tValue curve firstDerivativeValue secondDerivativeValue

tangentDirectionRange ::
  CurveExists dimension units space =>
  Interval Unitless ->
  Curve dimension units space ->
  DirectionBounds dimension space
tangentDirectionRange tRange curve = do
  let derivativeRange_ = derivativeRange tRange curve
  let secondDerivativeRange_ = secondDerivativeRange tRange curve
  tangentDirectionRangeImpl tRange curve derivativeRange_ secondDerivativeRange_

tangentDirectionImpl ::
  CurveExists dimension units space =>
  Number ->
  Curve dimension units space ->
  Vector dimension units space ->
  Vector dimension units space ->
  Direction dimension space
tangentDirectionImpl tValue curve firstDerivativeValue secondDerivativeValue =
  Vector.Nonzero.direction . Nonzero $
    if
      | tValue == 0.0 && hasDegenerateStart curve -> secondDerivativeValue
      | tValue == 1.0 && hasDegenerateEnd curve -> -secondDerivativeValue
      | otherwise -> firstDerivativeValue

tangentDirectionRangeImpl ::
  CurveExists dimension units space =>
  Interval Unitless ->
  Curve dimension units space ->
  VectorBounds dimension units space ->
  VectorBounds dimension units space ->
  DirectionBounds dimension space
tangentDirectionRangeImpl tRange curve firstDerivativeRange_ secondDerivativeRange_ =
  VectorBounds.direction $
    if
      | Interval.lower tRange == 0.0 && hasDegenerateStart curve -> secondDerivativeRange_
      | Interval.upper tRange == 1.0 && hasDegenerateEnd curve -> -secondDerivativeRange_
      | otherwise -> firstDerivativeRange_

curvatureAt_ ::
  (CurveExists dimension units space, Tolerance units) =>
  Number ->
  Curve dimension units space ->
  Maybe (Quantity (Unitless ?/? units), Direction dimension space)
curvatureAt_ tValue curve = do
  let f' = derivativeAt tValue curve
  let f'' = secondDerivativeAt tValue curve
  let tangent = tangentDirectionImpl tValue curve f' f''
  curvatureImpl_ tValue curve f' f'' tangent

curvatureVectorAt_ ::
  (CurveExists dimension units space, Tolerance units) =>
  Number ->
  Curve dimension units space ->
  Vector dimension (Unitless ?/? units) space
curvatureVectorAt_ tValue curve =
  case curvatureAt_ tValue curve of
    Nothing -> Vector.zero
    Just (curvatureMagnitude_, curvatureDirection) -> curvatureMagnitude_ * curvatureDirection

curvatureRange_ ::
  CurveExists dimension units space =>
  Interval Unitless ->
  Curve dimension units space ->
  (Interval (Unitless ?/? units), DirectionBounds dimension space)
curvatureRange_ tRange curve = do
  let f' = derivativeRange tRange curve
  let f'' = secondDerivativeRange tRange curve
  let tangent = tangentDirectionRangeImpl tRange curve f' f''
  curvatureRangeImpl_ f' f'' tangent

curvatureVectorRange_ ::
  CurveExists dimension units space =>
  Interval Unitless ->
  Curve dimension units space ->
  VectorBounds dimension (Unitless ?/? units) space
curvatureVectorRange_ tRange curve = do
  let (curvatureMagnitudeRange_, curvatureDirectionRange) = curvatureRange_ tRange curve
  curvatureMagnitudeRange_ * curvatureDirectionRange

curvatureImpl_ ::
  (CurveExists dimension units space, Tolerance units) =>
  Number ->
  Curve dimension units space ->
  Vector dimension units space ->
  Vector dimension units space ->
  Direction dimension space ->
  Maybe (Quantity (Unitless ?/? units), Direction dimension space)
curvatureImpl_ tValue curve f' f'' tangent =
  case Vector.magnitudeAndDirection (f'' - Vector.projectionIn tangent f'') of
    Err IsZero -> Nothing
    Ok (numerator, curvatureDirection) -> do
      let curvatureMagnitude =
            if isDegenerateAt tValue curve
              then Quantity.infinity
              else Units.simplify (numerator ?/? Vector.squaredMagnitude_ f')
      Just (curvatureMagnitude, curvatureDirection)

curvatureRangeImpl_ ::
  CurveExists dimension units space =>
  VectorBounds dimension units space ->
  VectorBounds dimension units space ->
  DirectionBounds dimension space ->
  (Interval (Unitless ?/? units), DirectionBounds dimension space)
curvatureRangeImpl_ f' f'' tangent = do
  let f''Perpendicular = f'' - tangent * (f'' `dot` tangent)
  let curvatureDirection = VectorBounds.direction f''Perpendicular
  let curvatureMagnitude = Units.simplify do
        VectorBounds.magnitude f''Perpendicular ?/? VectorBounds.squaredMagnitude_ f'
  (curvatureMagnitude, curvatureDirection)

bisectionTree :: Curve dimension units space -> BisectionTree dimension units space
bisectionTree = (.bisectionTree)

hasDegenerateStart :: CurveExists dimension units space => Curve dimension units space -> Bool
hasDegenerateStart = (.hasDegenerateStart)

hasDegenerateEnd :: CurveExists dimension units space => Curve dimension units space -> Bool
hasDegenerateEnd = (.hasDegenerateEnd)

isOnAxis ::
  (CurveExists dimension units space, Tolerance units) =>
  Axis dimension units space ->
  Curve dimension units space ->
  Bool
isOnAxis axis curve = NonEmpty.all (^ axis) (testPoints curve)

nonzero ::
  (CurveExists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Result HasDegeneracy (Nonzero (Curve dimension units space))
nonzero curve =
  if startDerivative curve ~= Vector.zero || endDerivative curve ~= Vector.zero
    then Err HasDegeneracy
    else Ok (Nonzero curve)

startDerivative ::
  CurveExists dimension units space =>
  Curve dimension units space ->
  Vector dimension units space
startDerivative curve = VectorCurve.startValue (derivative curve)

endDerivative ::
  CurveExists dimension units space =>
  Curve dimension units space ->
  Vector dimension units space
endDerivative curve = VectorCurve.endValue (derivative curve)

derivativeAt ::
  CurveExists dimension units space =>
  Number ->
  Curve dimension units space ->
  Vector dimension units space
derivativeAt tValue curve = VectorCurve.valueAt tValue (derivative curve)

derivativeRange ::
  CurveExists dimension units space =>
  Interval Unitless ->
  Curve dimension units space ->
  VectorBounds dimension units space
derivativeRange tRange curve = VectorCurve.range tRange (derivative curve)

startSecondDerivative ::
  CurveExists dimension units space =>
  Curve dimension units space ->
  Vector dimension units space
startSecondDerivative curve = VectorCurve.startValue (secondDerivative curve)

endSecondDerivative ::
  CurveExists dimension units space =>
  Curve dimension units space ->
  Vector dimension units space
endSecondDerivative curve = VectorCurve.endValue (secondDerivative curve)

secondDerivativeAt ::
  CurveExists dimension units space =>
  Number ->
  Curve dimension units space ->
  Vector dimension units space
secondDerivativeAt tValue curve = VectorCurve.valueAt tValue (secondDerivative curve)

secondDerivativeRange ::
  CurveExists dimension units space =>
  Interval Unitless ->
  Curve dimension units space ->
  VectorBounds dimension units space
secondDerivativeRange tRange curve = VectorCurve.range tRange (secondDerivative curve)

reverse ::
  CurveExists dimension units space =>
  Curve dimension units space ->
  Curve dimension units space
reverse curve =
  recursive \reversed ->
    Curve
      { compiled = compiled curve << Curve1D.compiled (1.0 - Curve1D.t)
      , derivative = negate (VectorCurve.reverse (derivative curve))
      , startPoint = curve.endPoint
      , endPoint = curve.startPoint
      , bounds = curve.bounds
      , hasDegenerateStart = curve.hasDegenerateEnd
      , hasDegenerateEnd = curve.hasDegenerateStart
      , bisectionTree = buildBisectionTree Interval.unit reversed
      , arcLengthParameterization =
          Pair.mapSecond (\f r -> 1.0 - f (1.0 - r)) curve.arcLengthParameterization
      }

distanceAlong ::
  CurveExists dimension units space =>
  Axis dimension units space ->
  Curve dimension units space ->
  Curve1D units
distanceAlong axis curve = (curve - Axis.originPoint axis) `dot` Axis.direction axis

affixWidth :: Number
affixWidth = 1 / 256

desingularizeStart ::
  CurveExists dimension units space =>
  Point dimension units space ->
  Vector dimension units space ->
  Curve dimension units space ->
  (Curve dimension units space, Curve dimension units space)
desingularizeStart givenStartPoint givenStartDerivative curve =
  Tolerance.using Quantity.zero do
    let panic = error "Desingularization should never produce degenerate curve"
    let tInner = affixWidth
    let prefix =
          hermite
            givenStartPoint
            [affixWidth * givenStartDerivative]
            (pointAt tInner curve)
            [ affixWidth * derivativeAt tInner curve
            , affixWidth * affixWidth * secondDerivativeAt tInner curve
            ]
            ! panic
    let suffix = curve << Curve1D.interpolateFrom tInner 1.0 ! panic
    (prefix, suffix)

desingularizeEnd ::
  CurveExists dimension units space =>
  Curve dimension units space ->
  Point dimension units space ->
  Vector dimension units space ->
  (Curve dimension units space, Curve dimension units space)
desingularizeEnd curve givenEndPoint givenEndDerivative =
  Tolerance.using Quantity.zero do
    let panic = error "Desingularization should never produce degenerate curve"
    let tInner = 1.0 - affixWidth
    let prefix = curve << Curve1D.interpolateFrom 0.0 tInner ! panic
    let suffix =
          hermite
            (pointAt tInner curve)
            [ affixWidth * derivativeAt tInner curve
            , affixWidth * affixWidth * secondDerivativeAt tInner curve
            ]
            givenEndPoint
            [affixWidth * givenEndDerivative]
            ! panic
    (prefix, suffix)

data Monotonic = Monotonic deriving (Eq)

findPoint ::
  (CurveExists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Curve dimension units space ->
  List Number
findPoint givenPoint givenCurve = do
  let endpointSolutions = [t | t <- [0.0, 1.0], pointAt t givenCurve ~= givenPoint]
  let endpointSolutionSet = Bag.pack endpointSolutions
  let isDistant segment = not (givenPoint ^ Curve.Segment.range segment)
  let resolvedMonotonicity _ segment
        | isDistant segment = Resolved Nothing
        | Curve.Segment.isMonotonic segment = Resolved (Just Monotonic)
        | Curve.Segment.isDegenerate segment = Resolved (Just Monotonic)
        | otherwise = Unresolved
  let evaluate tValue =
        (# pointAt tValue givenCurve - givenPoint, derivativeAt tValue givenCurve #)
  let validateSolution tValue =
        if pointAt tValue givenCurve ~= givenPoint then Just tValue else Nothing
  let resolvedSolution Monotonic tRange segment
        | isDistant segment = Resolved Nothing
        | otherwise = Fuzzy.map validateSolution (NewtonRaphson.Curve.solveIn tRange evaluate)
  let clusters =
        bisectionTree givenCurve
          & Bisection.clusters endpointSolutionSet resolvedMonotonicity
  let interiorSolutions = List.filterMap (Bisection.find resolvedSolution) clusters
  List.sort (endpointSolutions <> interiorSolutions)

isDegenerateAt ::
  (CurveExists dimension units space, Tolerance units) =>
  Number ->
  Curve dimension units space ->
  Bool
isDegenerateAt 0.0 curve = hasDegenerateStart curve
isDegenerateAt 1.0 curve = hasDegenerateEnd curve
isDegenerateAt _ _ = False -- Assume no interior degeneracies

continuityAt ::
  forall dimension units space.
  (CurveExists dimension units space, Tolerance units) =>
  (Number, Number) ->
  (Curve dimension units space, Curve dimension units space) ->
  Maybe Continuity
continuityAt (t1, t2) (curve1, curve2)
  | pointAt t1 curve1 ~= pointAt t2 curve2 = do
      let firstDerivative1 = derivativeAt t1 curve1
      let firstDerivative2 = derivativeAt t2 curve2
      let secondDerivative1 = secondDerivativeAt t1 curve1
      let secondDerivative2 = secondDerivativeAt t2 curve2
      let tangent1 = tangentDirectionImpl t1 curve1 firstDerivative1 secondDerivative1
      let tangent2 = tangentDirectionImpl t2 curve2 firstDerivative2 secondDerivative2
      if Direction.areIndependent tangent1 tangent2
        then Just Continuity.Crossing
        else do
          let alignment = Number.sign (tangent1 `dot` tangent2)
          if isDegenerateAt t1 curve1 || isDegenerateAt t2 curve2
            then Just (Continuity.Indistinguishable alignment)
            else do
              let scale1 = Vector.magnitude firstDerivative1
              let scale2 = Vector.magnitude firstDerivative2
              let maybeCurvature1_ =
                    curvatureImpl_ t1 curve1 firstDerivative1 secondDerivative1 tangent1
              let maybeCurvature2_ =
                    curvatureImpl_ t2 curve2 firstDerivative2 secondDerivative2 tangent2
              if matchingCurvatures (min scale1 scale2) maybeCurvature1_ maybeCurvature2_
                then Just (Continuity.Indistinguishable alignment)
                else Just (Continuity.Tangent alignment)
  | otherwise = Nothing

matchingCurvatures ::
  (CurveExists dimension units space, Tolerance units) =>
  Quantity units ->
  Maybe (Quantity (Unitless ?/? units), Direction dimension space) ->
  Maybe (Quantity (Unitless ?/? units), Direction dimension space) ->
  Bool
matchingCurvatures _ Nothing Nothing = True
matchingCurvatures _ Nothing (Just _) = False
matchingCurvatures _ (Just _) Nothing = False
matchingCurvatures scale (Just (magnitude1, direction1)) (Just (magnitude2, direction2)) =
  unitless (direction1 ~= direction2) && do
    let relativeCurvature = Quantity.abs (magnitude1 - magnitude2)
    let curvatureError = Units.simplify (relativeCurvature ?*? scale ?*? scale)
    curvatureError ~= Quantity.zero

intersections ::
  ( CurveExists dimension units space
  , NewtonRaphson.Surface.Solver dimension units space
  , Tolerance units
  ) =>
  Curve dimension units space ->
  Curve dimension units space ->
  Maybe Intersections
intersections = Curve.Intersections.intersections

linearDeviation ::
  CurveExists dimension units space =>
  Curve dimension units space ->
  Interval Unitless ->
  Quantity units
linearDeviation curve (Interval t1 t2) = do
  let p1 = pointAt t1 curve
  let p2 = pointAt t2 curve
  let pMid = pointAt (Number.midpoint t1 t2) curve
  let midError = Line.distanceTo pMid (Line p1 p2)
  max midError (leftRightError curve t1 t2 p1 p2)

toPolyline ::
  CurveExists dimension units space =>
  Resolution units ->
  Curve dimension units space ->
  Polyline dimension units space
toPolyline resolution curve =
  Polyline (NonEmpty.map (pointOn curve) (linearize resolution curve))

linearize ::
  CurveExists dimension units space =>
  Resolution units ->
  Curve dimension units space ->
  NonEmpty Number
linearize resolution curve = do
  let collect (Interval t1 t2) p1 p2 accumulated = do
        let tMid = Number.midpoint t1 t2
        let pMid = pointAt tMid curve
        let midError = Line.distanceTo pMid (Line p1 p2)
        let linearizationError = max midError (leftRightError curve t1 t2 p1 p2)
        let size = Point.distanceFrom p1 p2
        if Resolution.acceptable ("size" ::: size) ("error" ::: linearizationError) resolution
          then NonEmpty.push t1 accumulated
          else
            accumulated
              & collect (Interval tMid t2) pMid p2
              & collect (Interval t1 tMid) p1 pMid
  collect Interval.unit (startPoint curve) (endPoint curve) (NonEmpty.one 1.0)

leftRightError ::
  CurveExists dimension units space =>
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
  let leftError = Line.distanceTo (pointAt tLeft curve) (Line p1 p2)
  let rightError = Line.distanceTo (pointAt tRight curve) (Line p1 p2)
  max leftError rightError

arcLengthParameterization ::
  CurveExists dimension units space =>
  Curve dimension units space ->
  (Quantity units, Number -> Number)
arcLengthParameterization = (.arcLengthParameterization)

length ::
  CurveExists dimension units space =>
  Curve dimension units space ->
  Quantity units
length = Pair.first . arcLengthParameterization

uniformParameterization ::
  CurveExists dimension units space =>
  Curve dimension units space ->
  Number ->
  Number
uniformParameterization = Pair.second . arcLengthParameterization

fromUniform ::
  CurveExists dimension units space =>
  Number ->
  Curve dimension units space ->
  Number
fromUniform rValue curve = uniformParameterization curve rValue

atUniform ::
  CurveExists dimension units space =>
  Number ->
  Curve dimension units space ->
  Point dimension units space
atUniform r curve = pointAt (uniformParameterization curve r) curve

transformBy ::
  (CurveExists dimension units space, Transform.Tag.IsOrthonormal tag) =>
  Transform dimension tag units space ->
  Curve dimension units space ->
  Curve dimension units space
transformBy transform = do
  let transformCompiled =
        CompiledFunction.map
          (Expression.transformBy (Transform.asOrthonormal transform))
          (Point.transformBy transform)
          (Bounds.transformBy transform)
  let transformDerivative =
        VectorCurve.transformBy (Transform.vectorTransform transform)
  orthonormalTransform transformCompiled transformDerivative

orthonormalTransform ::
  (CurveExists dimension1 units space1, CurveExists dimension2 units space2) =>
  (Compiled dimension1 units space1 -> Compiled dimension2 units space2) ->
  (VectorCurve dimension1 units space1 -> VectorCurve dimension2 units space2) ->
  Curve dimension1 units space1 ->
  Curve dimension2 units space2
orthonormalTransform transformCompiled transformDerivative curve =
  recursive \transformed -> do
    let transformedCompiled = transformCompiled curve.compiled
    let transformedDerivative = transformDerivative curve.derivative
    Curve
      { compiled = transformedCompiled
      , derivative = transformedDerivative
      , startPoint = CompiledFunction.value 0.0 transformedCompiled
      , endPoint = CompiledFunction.value 1.0 transformedCompiled
      , bounds = CompiledFunction.range Interval.unit transformedCompiled
      , hasDegenerateStart = curve.hasDegenerateStart
      , hasDegenerateEnd = curve.hasDegenerateEnd
      , bisectionTree = buildBisectionTree Interval.unit transformed
      , arcLengthParameterization = curve.arcLengthParameterization
      }

convert :: Quantity (units2 ?/? units1) -> Curve2D units1 -> Curve2D units2
convert factor curve =
  recursive \converted ->
    Curve
      { compiled =
          CompiledFunction.map
            (Expression.convert factor)
            (Point2D.convert factor)
            (Bounds2D.convert factor)
            (compiled curve)
      , derivative = VectorCurve2D.convert factor (derivative curve)
      , startPoint = Point2D.convert factor curve.startPoint
      , endPoint = Point2D.convert factor curve.endPoint
      , hasDegenerateStart = curve.hasDegenerateStart
      , hasDegenerateEnd = curve.hasDegenerateEnd
      , bounds = Bounds2D.convert factor curve.bounds
      , -- TODO just apply units conversion to the existing bisection tree
        bisectionTree = buildBisectionTree Interval.unit converted
      , arcLengthParameterization =
          Pair.mapFirst (Quantity.convert factor) curve.arcLengthParameterization
      }

placeOn :: Plane3D space -> Curve2D Meters -> Curve3D space
placeOn plane = do
  let transformCompiled =
        CompiledFunction.map
          (Expression.placeOn plane)
          (Point2D.placeOn plane)
          (Bounds2D.placeOn plane)
  let transformDerivative = VectorCurve3D.on plane
  orthonormalTransform transformCompiled transformDerivative

displaceBy ::
  ( CurveExists dimension1 units1 space1
  , Tolerance units1
  , dimension1 ~ dimension2
  , space1 ~ space2
  , units1 ~ units2
  ) =>
  VectorCurve dimension2 units2 space2 ->
  Curve dimension1 units1 space1 ->
  Result (IsDegenerate dimension1 units1 space1) (Curve dimension1 units1 space1)
displaceBy vectorCurve curve =
  new
    (compiled curve + VectorCurve.compiled vectorCurve)
    (derivative curve + VectorCurve.derivative vectorCurve)
