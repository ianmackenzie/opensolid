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

import GHC.Records (HasField (getField))
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
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Curve2d qualified as Expression.Curve2d
import OpenSolid.Expression.Curve3d qualified as Expression.Curve3d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Length (Length)
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
import OpenSolid.Units (HasUnits)
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.VectorCurve3d (VectorCurve3d)
import OpenSolid.VectorCurve3d qualified as VectorCurve3d

data Curve3d space = Curve3d (Compiled space) ~(VectorCurve3d space Meters)

type Compiled space =
  CompiledFunction Number (Point3d space Meters) (Bounds Unitless) (Bounds3d space Meters)

instance HasField "compiled" (Curve3d space) (Compiled space) where
  getField (Curve3d c _) = c

instance HasField "derivative" (Curve3d space) (VectorCurve3d space Meters) where
  getField (Curve3d _ d) = d

instance HasUnits (Curve3d space) Meters

instance Bounded3d (Curve3d space) space where
  bounds = bounds

data HasDegeneracy = HasDegeneracy deriving (Eq, Show)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition
    (Curve3d space1)
    (VectorCurve3d space2 meters)
    (Curve3d space1)
  where
  lhs .+. rhs = new (lhs.compiled .+. rhs.compiled) (lhs.derivative .+. rhs.derivative)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction
    (Curve3d space1)
    (VectorCurve3d space2 meters)
    (Curve3d space1)
  where
  lhs .-. rhs = new (lhs.compiled .-. rhs.compiled) (lhs.derivative .-. rhs.derivative)

instance
  space1 ~ space2 =>
  Subtraction
    (Curve3d space1)
    (Curve3d space2)
    (VectorCurve3d space1 Meters)
  where
  lhs .-. rhs = VectorCurve3d.new (lhs.compiled .-. rhs.compiled) (lhs.derivative .-. rhs.derivative)

instance Composition (Curve Unitless) (Curve3d space) (Curve3d space) where
  outer `compose` inner =
    new
      (outer.compiled `compose` inner.compiled)
      ((outer.derivative `compose` inner) .*. inner.derivative)

instance
  unitless ~ Unitless =>
  Composition (SurfaceFunction unitless) (Curve3d space) (SurfaceFunction3d space)
  where
  curve `compose` function =
    SurfaceFunction3d.new
      (curve.compiled `compose` function.compiled)
      (\p -> (curve.derivative `compose` function) .*. SurfaceFunction.derivative p function)

instance ApproximateEquality (Curve3d space) Meters where
  curve1 ~= curve2 = List.allTrue [evaluate curve1 t ~= evaluate curve2 t | t <- Parameter.samples]

new :: Compiled space -> VectorCurve3d space Meters -> Curve3d space
new = Curve3d

recursive :: Compiled space -> (Curve3d space -> VectorCurve3d space Meters) -> Curve3d space
recursive givenCompiled derivativeFunction =
  let result = new givenCompiled (derivativeFunction result) in result

constant :: Point3d space Meters -> Curve3d space
constant point = new (CompiledFunction.constant point) VectorCurve3d.zero

on :: Plane3d space Meters (Defines local) -> Curve2d local Meters -> Curve3d space
on plane curve2d = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.Curve2d.placeOn plane)
          (Point2d.placeOn plane)
          (Bounds2d.placeOn plane)
          curve2d.compiled
  new compiledPlaced (VectorCurve3d.on plane curve2d.derivative)

line :: Point3d space Meters -> Point3d space Meters -> Curve3d space
line p1 p2 = constant p1 .+. Curve.t .*. (p2 .-. p1)

{-| Construct a Bezier curve from its control points. For example,

> Curve3d.bezier (NonEmpty.four p1 p2 p3 p4))

will return a cubic Bezier curve with the given four control points.
-}
bezier :: NonEmpty (Point3d space Meters) -> Curve3d space
bezier controlPoints =
  new
    (CompiledFunction.concrete (Expression.bezierCurve controlPoints))
    (VectorCurve3d.bezier (Bezier.derivative controlPoints))

-- | Construct a quadratic Bezier curve from the given control points.
quadraticBezier ::
  Point3d space Meters ->
  Point3d space Meters ->
  Point3d space Meters ->
  Curve3d space
quadraticBezier p1 p2 p3 = bezier (NonEmpty.three p1 p2 p3)

-- | Construct a cubic Bezier curve from the given control points.
cubicBezier ::
  Point3d space Meters ->
  Point3d space Meters ->
  Point3d space Meters ->
  Point3d space Meters ->
  Curve3d space
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
  Point3d space Meters ->
  List (Vector3d space Meters) ->
  Point3d space Meters ->
  List (Vector3d space Meters) ->
  Curve3d space
hermite start startDerivatives end endDerivatives =
  bezier (Bezier.hermite start startDerivatives end endDerivatives)

startPoint :: Curve3d space -> Point3d space Meters
startPoint curve = evaluate curve 0

endPoint :: Curve3d space -> Point3d space Meters
endPoint curve = evaluate curve 1

evaluate :: Curve3d space -> Number -> Point3d space Meters
evaluate curve tValue = CompiledFunction.evaluate curve.compiled tValue

evaluateBounds :: Curve3d space -> Bounds Unitless -> Bounds3d space Meters
evaluateBounds curve tBounds = CompiledFunction.evaluateBounds curve.compiled tBounds

bounds :: Curve3d space -> Bounds3d space Meters
bounds curve = evaluateBounds curve Bounds.unitInterval

reverse :: Curve3d space -> Curve3d space
reverse curve = curve `compose` (1 -. Curve.t)

arcLengthParameterization :: Tolerance Meters => Curve3d space -> (Curve Unitless, Length)
arcLengthParameterization curve =
  ArcLength.parameterization (VectorCurve3d.magnitude curve.derivative)

parameterizeByArcLength :: Tolerance Meters => Curve3d space -> (Curve3d space, Length)
parameterizeByArcLength curve = do
  let (parameterization, length) = arcLengthParameterization curve
  (curve `compose` parameterization, length)

transformBy :: Transform3d tag space Meters -> Curve3d space -> Curve3d space
transformBy transform curve = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.Curve3d.transformBy transform)
          (Point3d.transformBy transform)
          (Bounds3d.transformBy transform)
          curve.compiled
  new compiledTransformed (VectorCurve3d.transformBy transform curve.derivative)

placeIn :: Frame3d global Meters (Defines local) -> Curve3d local -> Curve3d global
placeIn frame curve = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.Curve3d.placeIn frame)
          (Point3d.placeIn frame)
          (Bounds3d.placeIn frame)
          curve.compiled
  new compiledPlaced (VectorCurve3d.placeIn frame curve.derivative)

relativeTo :: Frame3d global Meters (Defines local) -> Curve3d global -> Curve3d local
relativeTo frame curve = placeIn (Frame3d.inverse frame) curve
