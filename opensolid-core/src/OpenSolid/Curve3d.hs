module OpenSolid.Curve3d
  ( Curve3d
  , HasDegeneracy (HasDegeneracy)
  , Compiled
  , new
  , recursive
  , constant
  , planar
  , line
  , bezier
  , quadraticBezier
  , cubicBezier
  , hermite
  , xyz
  , startPoint
  , endPoint
  , evaluate
  , evaluateBounds
  , bounds
  , compiled
  , derivative
  , reverse
  , arcLengthParameterization
  , unsafeArcLengthParameterization
  , parameterizeByArcLength
  , unsafeParameterizeByArcLength
  , transformBy
  )
where

import OpenSolid.ArcLength qualified as ArcLength
import OpenSolid.Bezier qualified as Bezier
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Bounds3d (Bounded3d, Bounds3d (Bounds3d))
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Error qualified as Error
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Curve2d qualified as Expression.Curve2d
import OpenSolid.Expression.Curve3d qualified as Expression.Curve3d
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Point3d (Point3d (Point3d))
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range
import OpenSolid.Result qualified as Result
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction3d (SurfaceFunction3d)
import OpenSolid.SurfaceFunction3d qualified as SurfaceFunction3d
import OpenSolid.Text qualified as Text
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.VectorCurve3d (VectorCurve3d)
import OpenSolid.VectorCurve3d qualified as VectorCurve3d
import Prelude qualified

data Curve3d (coordinateSystem :: CoordinateSystem) where
  Curve3d ::
    { compiled :: Compiled (space @ units)
    , derivative :: ~(VectorCurve3d (space @ units))
    } ->
    Curve3d (space @ units)

type Compiled (coordinateSystem :: CoordinateSystem) =
  CompiledFunction
    Float
    (Point3d coordinateSystem)
    (Range Unitless)
    (Bounds3d coordinateSystem)

instance Show (Curve3d (space @ units)) where
  show _ = Text.unpack "Curve3d"

instance HasUnits (Curve3d (space @ units)) units (Curve3d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion (Curve3d (space1 @ unitsA)) (Curve3d (space2 @ unitsB))
  where
  coerce Curve3d{compiled, derivative} =
    Curve3d
      { compiled = Units.coerce compiled
      , derivative = Units.coerce derivative
      }

instance Bounded3d (Curve3d (space @ units)) (space @ units) where
  bounds = bounds

data HasDegeneracy = HasDegeneracy deriving (Eq, Show, Error.Message)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Curve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve3d (space1 @ units1))
  where
  lhs + rhs =
    new (compiled lhs + VectorCurve3d.compiled rhs) (derivative lhs + VectorCurve3d.derivative rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve3d (space1 @ units1))
  where
  lhs - rhs =
    new (compiled lhs - VectorCurve3d.compiled rhs) (derivative lhs - VectorCurve3d.derivative rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve3d (space1 @ units1))
    (Curve3d (space2 @ units2))
    (VectorCurve3d (space1 @ units1))
  where
  lhs - rhs = VectorCurve3d.new (compiled lhs - compiled rhs) (derivative lhs - derivative rhs)

instance
  unitless ~ Unitless =>
  Composition (Curve unitless) (Curve3d (space @ units)) (Curve3d (space @ units))
  where
  outer . inner =
    new
      (compiled outer . Curve.compiled inner)
      ((derivative outer . inner) * Curve.derivative inner)

instance
  unitless ~ Unitless =>
  Composition
    (SurfaceFunction unitless)
    (Curve3d (space @ units))
    (SurfaceFunction3d (space @ units))
  where
  curve . function =
    SurfaceFunction3d.new
      (compiled curve . SurfaceFunction.compiled function)
      (\p -> derivative curve . function * SurfaceFunction.derivative p function)

instance
  (space1 ~ space2, units1 ~ units2) =>
  ApproximateEquality (Curve3d (space1 @ units1)) (Curve3d (space2 @ units2)) units1
  where
  curve1 ~= curve2 = List.allTrue [evaluate curve1 t ~= evaluate curve2 t | t <- Parameter.samples]

instance
  (space1 ~ space2, units1 ~ units2) =>
  ApproximateEquality (Curve3d (space1 @ units1)) (Point3d (space2 @ units2)) units1
  where
  curve ~= point = List.allTrue [evaluate curve t ~= point | t <- Parameter.samples]

new :: Compiled (space @ units) -> VectorCurve3d (space @ units) -> Curve3d (space @ units)
new = Curve3d

recursive ::
  Compiled (space @ units) ->
  (Curve3d (space @ units) -> VectorCurve3d (space @ units)) ->
  Curve3d (space @ units)
recursive givenCompiled derivativeFunction =
  let result = new givenCompiled (derivativeFunction result) in result

constant :: Point3d (space @ units) -> Curve3d (space @ units)
constant point = new (CompiledFunction.constant point) VectorCurve3d.zero

xyz :: Curve units -> Curve units -> Curve units -> Curve3d (space @ units)
xyz x y z =
  new
    & CompiledFunction.map3
      Expression.xyz
      Point3d
      Bounds3d
      (Curve.compiled x)
      (Curve.compiled y)
      (Curve.compiled z)
    & VectorCurve3d.xyz (Curve.derivative x) (Curve.derivative y) (Curve.derivative z)

planar ::
  Plane3d (space @ units) (Defines local) ->
  Curve2d (local @ units) ->
  Curve3d (space @ units)
planar plane curve2d = do
  new
    & CompiledFunction.map
      (Expression.Curve2d.placeOn plane)
      (Point2d.placeOn plane)
      (Bounds2d.placeOn plane)
      (Curve2d.compiled curve2d)
    & VectorCurve3d.planar (Plane3d.basis plane) (Curve2d.derivative curve2d)

line :: Point3d (space @ units) -> Point3d (space @ units) -> Curve3d (space @ units)
line p1 p2 = constant p1 + Curve.t * (p2 - p1)

{-| Construct a Bezier curve from its control points. For example,

> Curve3d.bezier (NonEmpty.four p1 p2 p3 p4))

will return a cubic Bezier curve with the given four control points.
-}
bezier :: NonEmpty (Point3d (space @ units)) -> Curve3d (space @ units)
bezier controlPoints =
  new
    & CompiledFunction.concrete (Expression.bezierCurve controlPoints)
    & VectorCurve3d.bezierCurve (Bezier.derivative controlPoints)

-- | Construct a quadratic Bezier curve from the given control points.
quadraticBezier ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Curve3d (space @ units)
quadraticBezier p1 p2 p3 = bezier (NonEmpty.three p1 p2 p3)

-- | Construct a cubic Bezier curve from the given control points.
cubicBezier ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Curve3d (space @ units)
cubicBezier p1 p2 p3 p4 = bezier (NonEmpty.four p1 p2 p3 p4)

{-| Construct a Bezier curve with the given start point, start derivatives, end point and end
derivatives. For example,

> Curve3d.hermite (p1, [v1]) (p2, [v2])

will result in a cubic spline from @p1@ to @p2@ with first derivative equal to @v1@ at @p1@ and
first derivative equal to @v2@ at @p2@.

The numbers of derivatives at each endpoint do not have to be equal; for example,

> Curve3d.hermite (p1, [v1]) (p2, [])

will result in a quadratic spline from @p1@ to @p2@ with first derivative at @p1@ equal to @v1@.

In general, the degree of the resulting spline will be equal to 1 plus the total number of
derivatives given.
-}
hermite ::
  Point3d (space @ units) ->
  List (Vector3d (space @ units)) ->
  Point3d (space @ units) ->
  List (Vector3d (space @ units)) ->
  Curve3d (space @ units)
hermite start startDerivatives end endDerivatives =
  bezier (Bezier.hermite start startDerivatives end endDerivatives)

startPoint :: Curve3d (space @ units) -> Point3d (space @ units)
startPoint curve = evaluate curve 0.0

endPoint :: Curve3d (space @ units) -> Point3d (space @ units)
endPoint curve = evaluate curve 1.0

evaluate :: Curve3d (space @ units) -> Float -> Point3d (space @ units)
evaluate Curve3d{compiled} tValue = CompiledFunction.evaluate compiled tValue

evaluateBounds :: Curve3d (space @ units) -> Range Unitless -> Bounds3d (space @ units)
evaluateBounds Curve3d{compiled} tRange = CompiledFunction.evaluateBounds compiled tRange

bounds :: Curve3d (space @ units) -> Bounds3d (space @ units)
bounds curve = evaluateBounds curve Range.unit

reverse :: Curve3d (space @ units) -> Curve3d (space @ units)
reverse curve = curve . (1.0 - Curve.t)

arcLengthParameterization ::
  Tolerance units =>
  Curve3d (space @ units) ->
  Result HasDegeneracy (Curve Unitless, Qty units)
arcLengthParameterization curve = do
  let curveDerivative = derivative curve
  if VectorCurve3d.isZero curveDerivative
    then Success (Curve.t, Qty.zero) -- Curve is a constant point
    else case VectorCurve3d.magnitude (derivative curve) of
      Failure VectorCurve3d.HasZero -> Failure HasDegeneracy
      Success derivativeMagnitude -> Success (ArcLength.parameterization derivativeMagnitude)

unsafeArcLengthParameterization :: Curve3d (space @ units) -> (Curve Unitless, Qty units)
unsafeArcLengthParameterization curve =
  ArcLength.parameterization (VectorCurve3d.unsafeMagnitude (derivative curve))

parameterizeByArcLength ::
  Tolerance units =>
  Curve3d (space @ units) ->
  Result HasDegeneracy (Curve3d (space @ units), Qty units)
parameterizeByArcLength curve = Result.do
  (parameterization, length) <- arcLengthParameterization curve
  Success (curve . parameterization, length)

unsafeParameterizeByArcLength :: Curve3d (space @ units) -> (Curve3d (space @ units), Qty units)
unsafeParameterizeByArcLength curve = do
  let (parameterization, length) = unsafeArcLengthParameterization curve
  (curve . parameterization, length)

transformBy ::
  Transform3d tag (space @ units) ->
  Curve3d (space @ units) ->
  Curve3d (space @ units)
transformBy transform curve =
  new
    & CompiledFunction.map
      (Expression.Curve3d.transformBy transform)
      (Point3d.transformBy transform)
      (Bounds3d.transformBy transform)
      (compiled curve)
    & VectorCurve3d.transformBy transform (derivative curve)
