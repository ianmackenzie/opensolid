module OpenSolid.Curve3D
  ( Curve3D
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
  , derivative
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
  , findPoint
  )
where

import GHC.Records (HasField (getField))
import OpenSolid.ArcLength qualified as ArcLength
import OpenSolid.Bezier qualified as Bezier
import OpenSolid.Bounded3D (Bounded3D)
import OpenSolid.Bounded3D qualified as Bounded3D
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve2D (Curve2D)
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.Length (Length)
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.VectorCurve3D (VectorCurve3D)
import OpenSolid.VectorCurve3D qualified as VectorCurve3D

data Curve3D space = Curve3D (Compiled space) ~(VectorCurve3D Meters space)

type Compiled space =
  CompiledFunction Number (Point3D space) (Interval Unitless) (Bounds3D space)

instance HasField "compiled" (Curve3D space) (Compiled space) where
  getField (Curve3D c _) = c

instance HasField "derivative" (Curve3D space) (VectorCurve3D Meters space) where
  getField (Curve3D _ d) = d

instance Bounded3D (Curve3D space) space where
  bounds = bounds

data HasDegeneracy = HasDegeneracy deriving (Eq, Show)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition
    (Curve3D space1)
    (VectorCurve3D meters space2)
    (Curve3D space1)
  where
  lhs .+. rhs =
    new
      (lhs.compiled .+. VectorCurve3D.compiled rhs)
      (lhs.derivative .+. VectorCurve3D.derivative rhs)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction
    (Curve3D space1)
    (VectorCurve3D meters space2)
    (Curve3D space1)
  where
  lhs .-. rhs =
    new
      (lhs.compiled .-. VectorCurve3D.compiled rhs)
      (lhs.derivative .-. VectorCurve3D.derivative rhs)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition
    (Curve3D space1)
    (Vector3D meters space2)
    (Curve3D space1)
  where
  lhs .+. rhs = lhs .+. VectorCurve3D.constant rhs

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction
    (Curve3D space1)
    (Vector3D meters space2)
    (Curve3D space1)
  where
  lhs .-. rhs = lhs .-. VectorCurve3D.constant rhs

instance
  space1 ~ space2 =>
  Subtraction
    (Curve3D space1)
    (Curve3D space2)
    (VectorCurve3D Meters space1)
  where
  lhs .-. rhs =
    VectorCurve3D.new
      (lhs.compiled .-. rhs.compiled)
      (lhs.derivative .-. rhs.derivative)

instance
  space1 ~ space2 =>
  Subtraction
    (Curve3D space1)
    (Point3D space2)
    (VectorCurve3D Meters space1)
  where
  lhs .-. rhs = lhs .-. constant rhs

instance
  space1 ~ space2 =>
  Subtraction
    (Point3D space1)
    (Curve3D space2)
    (VectorCurve3D Meters space1)
  where
  lhs .-. rhs = constant lhs .-. rhs

instance Composition (Curve1D Unitless) (Curve3D space) (Curve3D space) where
  outer `compose` inner =
    new
      (outer.compiled `compose` inner.compiled)
      ((outer.derivative `compose` inner) .*. inner.derivative)

instance
  unitless ~ Unitless =>
  Composition (SurfaceFunction1D unitless) (Curve3D space) (SurfaceFunction3D space)
  where
  curve `compose` function =
    SurfaceFunction3D.new
      (curve.compiled `compose` function.compiled)
      (\p -> (curve.derivative `compose` function) .*. SurfaceFunction1D.derivative p function)

instance ApproximateEquality (Curve3D space) Meters where
  curve1 ~= curve2 = do
    let equalPoints t = evaluate curve1 t ~= evaluate curve2 t
    NonEmpty.allSatisfy equalPoints Parameter.samples

new :: Compiled space -> VectorCurve3D Meters space -> Curve3D space
new = Curve3D

recursive :: Compiled space -> (Curve3D space -> VectorCurve3D Meters space) -> Curve3D space
recursive givenCompiled derivativeFunction =
  let result = new givenCompiled (derivativeFunction result) in result

constant :: Point3D space -> Curve3D space
constant point = new (CompiledFunction.constant point) VectorCurve3D.zero

on :: Plane3D global local -> Curve2D Meters local -> Curve3D global
on plane curve2D = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.on plane)
          (Point2D.placeOn plane)
          (Bounds2D.placeOn plane)
          (Curve2D.compiled curve2D)
  new compiledPlaced (VectorCurve3D.on plane (Curve2D.derivative curve2D))

line :: Point3D space -> Point3D space -> Curve3D space
line p1 p2 = constant p1 .+. Curve1D.t .*. (p2 .-. p1)

{-| Construct a Bezier curve from its control points. For example,

> Curve3D.bezier (NonEmpty.four p1 p2 p3 p4))

will return a cubic Bezier curve with the given four control points.
-}
bezier :: NonEmpty (Point3D space) -> Curve3D space
bezier controlPoints =
  new
    (CompiledFunction.concrete (Expression.bezierCurve controlPoints))
    (VectorCurve3D.bezier (Bezier.derivative controlPoints))

-- | Construct a quadratic Bezier curve from the given control points.
quadraticBezier :: Point3D space -> Point3D space -> Point3D space -> Curve3D space
quadraticBezier p1 p2 p3 = bezier (NonEmpty.three p1 p2 p3)

-- | Construct a cubic Bezier curve from the given control points.
cubicBezier :: Point3D space -> Point3D space -> Point3D space -> Point3D space -> Curve3D space
cubicBezier p1 p2 p3 p4 = bezier (NonEmpty.four p1 p2 p3 p4)

{-| Construct a Bezier curve with the given start point, start derivatives, end point and end
derivatives. For example,

> Curve3D.hermite (p1, [v1]) (p2, [v2])

will result in a cubic spline from @p1@ to @p2@ with first derivative equal to @v1@ at @p1@ and
first derivative equal to @v2@ at @p2@.

The numbers of derivatives at each endpoint do not have to be equal; for example,

> Curve3D.hermite (p1, [v1]) (p2, [])

will result in a quadratic spline from @p1@ to @p2@ with first derivative at @p1@ equal to @v1@.

In general, the degree of the resulting spline will be equal to 1 plus the total number of
derivatives given.
-}
hermite ::
  Point3D space ->
  List (Vector3D Meters space) ->
  Point3D space ->
  List (Vector3D Meters space) ->
  Curve3D space
hermite start startDerivatives end endDerivatives =
  bezier (Bezier.hermite start startDerivatives end endDerivatives)

derivative :: Curve3D space -> VectorCurve3D Meters space
derivative = (.derivative)

startPoint :: Curve3D space -> Point3D space
startPoint curve = evaluate curve 0

endPoint :: Curve3D space -> Point3D space
endPoint curve = evaluate curve 1

evaluate :: Curve3D space -> Number -> Point3D space
evaluate curve tValue = CompiledFunction.evaluate curve.compiled tValue

evaluateBounds :: Curve3D space -> Interval Unitless -> Bounds3D space
evaluateBounds curve tBounds = CompiledFunction.evaluateBounds curve.compiled tBounds

bounds :: Curve3D space -> Bounds3D space
bounds curve = evaluateBounds curve Interval.unit

reverse :: Curve3D space -> Curve3D space
reverse curve = curve `compose` (1 -. Curve1D.t)

arcLengthParameterization :: Tolerance Meters => Curve3D space -> (Curve1D Unitless, Length)
arcLengthParameterization curve =
  ArcLength.parameterization (VectorCurve3D.magnitude curve.derivative)

parameterizeByArcLength :: Tolerance Meters => Curve3D space -> (Curve3D space, Length)
parameterizeByArcLength curve = do
  let (parameterization, length) = arcLengthParameterization curve
  (curve `compose` parameterization, length)

transformBy :: Transform3D tag space -> Curve3D space -> Curve3D space
transformBy transform curve = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.transformBy transform)
          (Point3D.transformBy transform)
          (Bounds3D.transformBy transform)
          curve.compiled
  new compiledTransformed (VectorCurve3D.transformBy transform curve.derivative)

placeIn :: Frame3D global local -> Curve3D local -> Curve3D global
placeIn frame curve = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.placeIn frame)
          (Point3D.placeIn frame)
          (Bounds3D.placeIn frame)
          curve.compiled
  new compiledPlaced (VectorCurve3D.placeIn frame curve.derivative)

relativeTo :: Frame3D global local -> Curve3D global -> Curve3D local
relativeTo frame curve = placeIn (Frame3D.inverse frame) curve

findPoint ::
  Tolerance Meters =>
  Point3D space ->
  Curve3D space ->
  Result Curve.IsPoint (List Number)
findPoint = Curve.findPoint
