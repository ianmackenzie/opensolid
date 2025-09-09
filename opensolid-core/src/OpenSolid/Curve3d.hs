module OpenSolid.Curve3d
  ( Curve3d
  , HasDegeneracy (HasDegeneracy)
  , Compiled
  , new
  , recursive
  , constant
  , on
  , line
  , bezier
  , quadraticBezier
  , cubicBezier
  , hermite
  , startPoint
  , endPoint
  , evaluate
  , evaluateBounds
  , bounds
  , reverse
  , arcLengthParameterization
  , parameterizeByArcLength
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.ArcLength qualified as ArcLength
import OpenSolid.Bezier qualified as Bezier
import OpenSolid.Bounded3d (Bounded3d)
import OpenSolid.Bounded3d qualified as Bounded3d
import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Bounds3d (Bounds3d)
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Error qualified as Error
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Curve2d qualified as Expression.Curve2d
import OpenSolid.Expression.Curve3d qualified as Expression.Curve3d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction3d (SurfaceFunction3d)
import OpenSolid.SurfaceFunction3d qualified as SurfaceFunction3d
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.VectorCurve3d (VectorCurve3d)
import OpenSolid.VectorCurve3d qualified as VectorCurve3d

data Curve3d (coordinateSystem :: CoordinateSystem) where
  Curve3d :: Compiled (space @ units) -> ~(VectorCurve3d (space @ units)) -> Curve3d (space @ units)

type Compiled (coordinateSystem :: CoordinateSystem) =
  CompiledFunction
    Float
    (Point3d coordinateSystem)
    (Bounds Unitless)
    (Bounds3d coordinateSystem)

instance HasField "compiled" (Curve3d (space @ units)) (Compiled (space @ units)) where
  getField (Curve3d c _) = c

instance HasField "derivative" (Curve3d (space @ units)) (VectorCurve3d (space @ units)) where
  getField (Curve3d _ d) = d

instance HasUnits (Curve3d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (Curve3d (space1 @ unitsA)) (Curve3d (space2 @ unitsB))
  where
  coerce curve = Curve3d (Units.coerce curve.compiled) (Units.coerce curve.derivative)

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
  lhs + rhs = new (lhs.compiled + rhs.compiled) (lhs.derivative + rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve3d (space1 @ units1))
  where
  lhs - rhs = new (lhs.compiled - rhs.compiled) (lhs.derivative - rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve3d (space1 @ units1))
    (Curve3d (space2 @ units2))
    (VectorCurve3d (space1 @ units1))
  where
  lhs - rhs = VectorCurve3d.new (lhs.compiled - rhs.compiled) (lhs.derivative - rhs.derivative)

instance Composition (Curve Unitless) (Curve3d (space @ units)) (Curve3d (space @ units)) where
  outer . inner =
    new
      @ outer.compiled . inner.compiled
      @ (outer.derivative . inner) * inner.derivative

instance
  Composition
    (SurfaceFunction Unitless)
    (Curve3d (space @ units))
    (SurfaceFunction3d (space @ units))
  where
  curve . function =
    SurfaceFunction3d.new
      @ curve.compiled . function.compiled
      @ \p -> (curve.derivative . function) * SurfaceFunction.derivative p function

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

on ::
  Plane3d (space @ units) (Defines local) ->
  Curve2d (local @ units) ->
  Curve3d (space @ units)
on plane curve2d = do
  new
    @ CompiledFunction.map
      (Expression.Curve2d.on plane)
      (Point2d.on plane)
      (Bounds2d.on plane)
      curve2d.compiled
    @ VectorCurve3d.on plane curve2d.derivative

line :: Point3d (space @ units) -> Point3d (space @ units) -> Curve3d (space @ units)
line p1 p2 = constant p1 + Curve.t * (p2 - p1)

{-| Construct a Bezier curve from its control points. For example,

> Curve3d.bezier (NonEmpty.four p1 p2 p3 p4))

will return a cubic Bezier curve with the given four control points.
-}
bezier :: NonEmpty (Point3d (space @ units)) -> Curve3d (space @ units)
bezier controlPoints =
  new
    @ CompiledFunction.concrete (Expression.bezierCurve controlPoints)
    @ VectorCurve3d.bezier (Bezier.derivative controlPoints)

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
evaluate curve tValue = CompiledFunction.evaluate curve.compiled tValue

evaluateBounds :: Curve3d (space @ units) -> Bounds Unitless -> Bounds3d (space @ units)
evaluateBounds curve tBounds = CompiledFunction.evaluateBounds curve.compiled tBounds

bounds :: Curve3d (space @ units) -> Bounds3d (space @ units)
bounds curve = evaluateBounds curve Bounds.unitInterval

reverse :: Curve3d (space @ units) -> Curve3d (space @ units)
reverse curve = curve . (1.0 - Curve.t)

arcLengthParameterization ::
  Tolerance units =>
  Curve3d (space @ units) ->
  (Curve Unitless, Qty units)
arcLengthParameterization curve =
  ArcLength.parameterization (VectorCurve3d.magnitude curve.derivative)

parameterizeByArcLength ::
  Tolerance units =>
  Curve3d (space @ units) ->
  (Curve3d (space @ units), Qty units)
parameterizeByArcLength curve = do
  let (parameterization, length) = arcLengthParameterization curve
  (curve . parameterization, length)

transformBy ::
  Transform3d tag (space @ units) ->
  Curve3d (space @ units) ->
  Curve3d (space @ units)
transformBy transform curve =
  new
    @ CompiledFunction.map
      (Expression.Curve3d.transformBy transform)
      (Point3d.transformBy transform)
      (Bounds3d.transformBy transform)
      curve.compiled
    @ VectorCurve3d.transformBy transform curve.derivative

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Curve3d (local @ units) ->
  Curve3d (global @ units)
placeIn frame curve =
  new
    @ CompiledFunction.map
      (Expression.Curve3d.placeIn frame)
      (Point3d.placeIn frame)
      (Bounds3d.placeIn frame)
      curve.compiled
    @ VectorCurve3d.placeIn frame curve.derivative

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Curve3d (global @ units) ->
  Curve3d (local @ units)
relativeTo frame curve = placeIn (Frame3d.inverse frame) curve
