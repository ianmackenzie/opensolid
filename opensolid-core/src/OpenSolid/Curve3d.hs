module OpenSolid.Curve3d
  ( Curve3d (Parametric)
  , Interface (..)
  , HasDegeneracy (HasDegeneracy)
  , new
  , constant
  , line
  , bezier
  , quadraticBezier
  , cubicBezier
  , hermite
  , parametric
  , xyz
  , startPoint
  , endPoint
  , evaluate
  , evaluateBounds
  , bounds
  , derivative
  , reverse
  , arcLengthParameterization
  , unsafeArcLengthParameterization
  , parameterizeByArcLength
  , unsafeParameterizeByArcLength
  )
where

import OpenSolid.ArcLength qualified as ArcLength
import OpenSolid.Bounds3d (Bounded3d, Bounds3d)
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.Composition
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Error qualified as Error
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
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
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorCurve3d (VectorCurve3d)
import OpenSolid.VectorCurve3d qualified as VectorCurve3d

class
  Show curve =>
  Interface curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  evaluateImpl :: curve -> Float -> Point3d coordinateSystem
  evaluateBoundsImpl :: curve -> Range Unitless -> Bounds3d coordinateSystem
  derivativeImpl :: curve -> VectorCurve3d coordinateSystem
  reverseImpl :: curve -> Curve3d coordinateSystem

data Curve3d (coordinateSystem :: CoordinateSystem) where
  Curve ::
    Interface function (space @ units) =>
    function ->
    Curve3d (space @ units)
  Parametric ::
    Expression Float (Point3d (space @ units)) ->
    Curve3d (space @ units)
  Coerce ::
    Curve3d (space @ units1) ->
    Curve3d (space @ units2)
  XYZ ::
    Curve units ->
    Curve units ->
    Curve units ->
    Curve3d (space @ units)
  Addition ::
    Curve3d (space @ units) ->
    VectorCurve3d (space @ units) ->
    Curve3d (space @ units)
  Subtraction ::
    Curve3d (space @ units) ->
    VectorCurve3d (space @ units) ->
    Curve3d (space @ units)

deriving instance Show (Curve3d (space @ units))

instance HasUnits (Curve3d (space @ units)) units (Curve3d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion (Curve3d (space1 @ unitsA)) (Curve3d (space2 @ unitsB))
  where
  coerce f = case f of
    Parametric expression -> Parametric (Units.coerce expression)
    Coerce function -> Coerce function
    function -> Coerce function

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
  Parametric lhs + VectorCurve3d.Parametric rhs = Parametric (lhs + rhs)
  lhs + rhs = Addition lhs rhs

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve3d (space1 @ units1))
  where
  Parametric lhs - VectorCurve3d.Parametric rhs = Parametric (lhs - rhs)
  lhs - rhs = Subtraction lhs rhs

instance
  unitless ~ Unitless =>
  Composition (Curve unitless) (Curve3d (space @ units)) (Curve3d (space @ units))
  where
  Parametric outer . Curve.Parametric inner = Parametric (outer . inner)
  outer . inner = new (outer :.: inner)

instance
  unitless ~ Unitless =>
  Interface (Curve3d (space @ units) :.: Curve unitless) (space @ units)
  where
  evaluateImpl (curve3d :.: curve1d) tRange =
    evaluate curve3d (Curve.evaluate curve1d tRange)

  evaluateBoundsImpl (curve3d :.: curve1d) tRange =
    evaluateBounds curve3d (Curve.evaluateBounds curve1d tRange)

  derivativeImpl (curve3d :.: curve1d) =
    (derivative curve3d . curve1d) * Curve.derivative curve1d

  reverseImpl (curve3d :.: curve1d) =
    new (curve3d :.: Curve.reverse curve1d)

instance
  unitless ~ Unitless =>
  Composition
    (SurfaceFunction unitless)
    (Curve3d (space @ units))
    (SurfaceFunction3d (space @ units))
  where
  Parametric curve . SurfaceFunction.Parametric function =
    SurfaceFunction3d.Parametric (curve . function)
  curveFunction . surfaceFunction = SurfaceFunction3d.new (curveFunction :.: surfaceFunction)

instance
  unitless ~ Unitless =>
  SurfaceFunction3d.Interface
    (Curve3d (space @ units) :.: SurfaceFunction unitless)
    (space @ units)
  where
  evaluateImpl (curve :.: surfaceFunction) uvPoint =
    evaluate curve $
      SurfaceFunction.evaluate surfaceFunction uvPoint

  evaluateBoundsImpl (curve :.: surfaceFunction) uvBounds =
    evaluateBounds curve $
      SurfaceFunction.evaluateBounds surfaceFunction uvBounds

  derivativeImpl parameter (curve :.: surfaceFunction) =
    (derivative curve . surfaceFunction)
      * SurfaceFunction.derivative parameter surfaceFunction

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

new :: Interface function (space @ units) => function -> Curve3d (space @ units)
new = Curve

constant :: Point3d (space @ units) -> Curve3d (space @ units)
constant = Parametric . Expression.constant

parametric :: Expression Float (Point3d (space @ units)) -> Curve3d (space @ units)
parametric = Parametric

xyz :: Curve units -> Curve units -> Curve units -> Curve3d (space @ units)
xyz (Curve.Parametric x) (Curve.Parametric y) (Curve.Parametric z) =
  Parametric (Expression.xyz x y z)
xyz x y z = XYZ x y z

line :: Point3d (space @ units) -> Point3d (space @ units) -> Curve3d (space @ units)
line p1 p2 = constant p1 + Curve.t * (p2 - p1)

{-| Construct a Bezier curve from its control points. For example,

> Curve3d.bezier (NonEmpty.four p1 p2 p3 p4))

will return a cubic Bezier curve with the given four control points.
-}
bezier :: NonEmpty (Point3d (space @ units)) -> Curve3d (space @ units)
bezier controlPoints = do
  let x = Curve.bezier (NonEmpty.map Point3d.xCoordinate controlPoints)
  let y = Curve.bezier (NonEmpty.map Point3d.yCoordinate controlPoints)
  let z = Curve.bezier (NonEmpty.map Point3d.zCoordinate controlPoints)
  XYZ x y z

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
  (Point3d (space @ units), List (Vector3d (space @ units))) ->
  (Point3d (space @ units), List (Vector3d (space @ units))) ->
  Curve3d (space @ units)
hermite (Point3d x1 y1 z1, derivatives1) (Point3d x2 y2 z2, derivatives2) = do
  let xDerivatives1 = List.map Vector3d.xComponent derivatives1
  let yDerivatives1 = List.map Vector3d.yComponent derivatives1
  let zDerivatives1 = List.map Vector3d.zComponent derivatives1
  let xDerivatives2 = List.map Vector3d.xComponent derivatives2
  let yDerivatives2 = List.map Vector3d.yComponent derivatives2
  let zDerivatives2 = List.map Vector3d.zComponent derivatives2
  let x = Curve.hermite (x1, xDerivatives1) (x2, xDerivatives2)
  let y = Curve.hermite (y1, yDerivatives1) (y2, yDerivatives2)
  let z = Curve.hermite (z1, zDerivatives1) (z2, zDerivatives2)
  XYZ x y z

startPoint :: Curve3d (space @ units) -> Point3d (space @ units)
startPoint curve = evaluate curve 0.0

endPoint :: Curve3d (space @ units) -> Point3d (space @ units)
endPoint curve = evaluate curve 1.0

evaluate :: Curve3d (space @ units) -> Float -> Point3d (space @ units)
evaluate f tValue = case f of
  Parametric expression -> Expression.evaluate expression tValue
  Curve curve -> evaluateImpl curve tValue
  Coerce curve -> Units.coerce (evaluate curve tValue)
  XYZ x y z ->
    Point3d.xyz (Curve.evaluate x tValue) (Curve.evaluate y tValue) (Curve.evaluate z tValue)
  Addition c v -> evaluate c tValue + VectorCurve3d.evaluate v tValue
  Subtraction c v -> evaluate c tValue - VectorCurve3d.evaluate v tValue

evaluateBounds :: Curve3d (space @ units) -> Range Unitless -> Bounds3d (space @ units)
evaluateBounds f tRange = case f of
  Parametric expression -> Expression.evaluateBounds expression tRange
  Curve curve -> evaluateBoundsImpl curve tRange
  Coerce curve -> Units.coerce (evaluateBounds curve tRange)
  XYZ x y z ->
    Bounds3d.xyz
      (Curve.evaluateBounds x tRange)
      (Curve.evaluateBounds y tRange)
      (Curve.evaluateBounds z tRange)
  Addition c v -> evaluateBounds c tRange + VectorCurve3d.evaluateBounds v tRange
  Subtraction c v -> evaluateBounds c tRange - VectorCurve3d.evaluateBounds v tRange

bounds :: Curve3d (space @ units) -> Bounds3d (space @ units)
bounds curve = evaluateBounds curve Range.unit

derivative :: Curve3d (space @ units) -> VectorCurve3d (space @ units)
derivative f = case f of
  Parametric expression -> VectorCurve3d.Parametric (Expression.curveDerivative expression)
  Curve curve -> derivativeImpl curve
  Coerce curve -> Units.coerce (derivative curve)
  XYZ x y z ->
    VectorCurve3d.xyz (Curve.derivative x) (Curve.derivative y) (Curve.derivative z)
  Addition c v -> derivative c + VectorCurve3d.derivative v
  Subtraction c v -> derivative c - VectorCurve3d.derivative v

reverse :: Curve3d (space @ units) -> Curve3d (space @ units)
reverse f = case f of
  Parametric expression -> Parametric (expression . Expression.r)
  Curve curve -> reverseImpl curve
  Coerce curve -> Units.coerce (reverse curve)
  XYZ x y z -> XYZ (Curve.reverse x) (Curve.reverse y) (Curve.reverse z)
  Addition c v -> reverse c + VectorCurve3d.reverse v
  Subtraction c v -> reverse c - VectorCurve3d.reverse v

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
