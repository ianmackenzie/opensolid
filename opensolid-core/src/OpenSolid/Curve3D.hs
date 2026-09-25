module OpenSolid.Curve3D
  ( Curve3D
  , Compiled
  , Segment
  , IsDegenerate
  , data IsDegenerate
  , IntersectionPointWithSurface
  , new
  , on
  , line
  , lineFrom
  , bezier
  , quadraticBezier
  , cubicBezier
  , hermite
  , compiled
  , derivative
  , secondDerivative
  , startDerivative
  , endDerivative
  , derivativeAt
  , derivativeRange
  , startSecondDerivative
  , endSecondDerivative
  , secondDerivativeAt
  , secondDerivativeRange
  , isPoint
  , startPoint
  , endPoint
  , endpoints
  , pointAt
  , pointOn
  , range
  , bounds
  , reverse
  , arcLengthParameterization
  , length
  , uniformParameterization
  , fromUniform
  , atUniform
  , transformBy
  , placeIn
  , relativeTo
  , findPoint
  , intersections
  )
where

import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve (Curve3D)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Intersections qualified as Curve.Intersections
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve3D.IntersectionPointWithSurface (IntersectionPointWithSurface)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Interval (Interval)
import OpenSolid.Length (Length)
import OpenSolid.Line3D (Line3D)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Transform.Tag qualified as Transform.Tag
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorCurve3D (VectorCurve3D)
import OpenSolid.VectorCurve3D qualified as VectorCurve3D

type Compiled space = Curve.Compiled 3 Meters space

type Segment space = Curve.Segment 3 Meters space

type IsDegenerate space = Curve.IsDegenerate 3 Meters space

{-# COMPLETE IsDegenerate #-}

pattern IsDegenerate :: Point3D space -> IsDegenerate space
pattern IsDegenerate point = Curve.IsDegenerate point

new :: Tolerance Meters => Compiled space -> VectorCurve3D Meters space -> Curve3D space
new = Curve.new

on :: Plane3D space -> Curve2D Meters -> Curve3D space
on = Curve.placeOn

line :: Tolerance Meters => Line3D space -> Curve3D space
line = Curve.line

lineFrom :: Tolerance Meters => Point3D space -> Point3D space -> Curve3D space
lineFrom = Curve.lineFrom

{-| Construct a Bezier curve from its control points. For example,

> Curve3D.bezier (NonEmpty.four p1 p2 p3 p4))

will return a cubic Bezier curve with the given four control points.
-}
bezier :: Tolerance Meters => NonEmpty (Point3D space) -> Curve3D space
bezier = Curve.bezier

-- | Construct a quadratic Bezier curve from the given control points.
quadraticBezier ::
  Tolerance Meters =>
  Point3D space ->
  Point3D space ->
  Point3D space ->
  Curve3D space
quadraticBezier = Curve.quadraticBezier

-- | Construct a cubic Bezier curve from the given control points.
cubicBezier ::
  Tolerance Meters =>
  Point3D space ->
  Point3D space ->
  Point3D space ->
  Point3D space ->
  Curve3D space
cubicBezier = Curve.cubicBezier

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
  Tolerance Meters =>
  Point3D space ->
  List (Vector3D Meters space) ->
  Point3D space ->
  List (Vector3D Meters space) ->
  Curve3D space
hermite = Curve.hermite

{-# INLINE derivative #-}
derivative :: Curve3D space -> VectorCurve3D Meters space
derivative = Curve.derivative

{-# INLINE compiled #-}
compiled :: Curve3D space -> Compiled space
compiled = Curve.compiled

secondDerivative :: Curve3D space -> VectorCurve3D Meters space
secondDerivative = Curve.secondDerivative

startDerivative :: Curve3D space -> Vector3D Meters space
startDerivative = Curve.startDerivative

endDerivative :: Curve3D space -> Vector3D Meters space
endDerivative = Curve.endDerivative

{-# INLINE derivativeAt #-}
derivativeAt :: Number -> Curve3D space -> Vector3D Meters space
derivativeAt = Curve.derivativeAt

{-# INLINE derivativeRange #-}
derivativeRange :: Interval Unitless -> Curve3D space -> VectorBounds3D Meters space
derivativeRange = Curve.derivativeRange

startSecondDerivative :: Curve3D space -> Vector3D Meters space
startSecondDerivative = Curve.startSecondDerivative

endSecondDerivative :: Curve3D space -> Vector3D Meters space
endSecondDerivative = Curve.endSecondDerivative

{-# INLINE secondDerivativeAt #-}
secondDerivativeAt :: Number -> Curve3D space -> Vector3D Meters space
secondDerivativeAt = Curve.secondDerivativeAt

{-# INLINE secondDerivativeRange #-}
secondDerivativeRange :: Interval Unitless -> Curve3D space -> VectorBounds3D Meters space
secondDerivativeRange = Curve.secondDerivativeRange

isPoint :: Tolerance Meters => Curve3D space -> Bool
isPoint = Curve.isPoint

startPoint :: Curve3D space -> Point3D space
startPoint = Curve.startPoint

endPoint :: Curve3D space -> Point3D space
endPoint = Curve.endPoint

endpoints :: Curve3D space -> (Point3D space, Point3D space)
endpoints = Curve.endpoints

pointAt :: Number -> Curve3D space -> Point3D space
pointAt = Curve.pointAt

pointOn :: Curve3D space -> Number -> Point3D space
pointOn = Curve.pointOn

range :: Interval Unitless -> Curve3D space -> Bounds3D space
range = Curve.range

bounds :: Curve3D space -> Bounds3D space
bounds = Curve.bounds

reverse :: Curve3D space -> Curve3D space
reverse = Curve.reverse

arcLengthParameterization :: Tolerance Meters => Curve3D space -> (Length, Number -> Number)
arcLengthParameterization = Curve.arcLengthParameterization

length :: Tolerance Meters => Curve3D space -> Length
length = Curve.length

uniformParameterization :: Tolerance Meters => Curve3D space -> Number -> Number
uniformParameterization = Curve.uniformParameterization

fromUniform :: Tolerance Meters => Number -> Curve3D space -> Number
fromUniform = Curve.fromUniform

atUniform :: Tolerance Meters => Number -> Curve3D space -> Point3D space
atUniform = Curve.atUniform

transformBy ::
  Transform.Tag.IsOrthonormal tag =>
  Transform3D tag space ->
  Curve3D space ->
  Curve3D space
transformBy = Curve.transformBy

placeIn :: Frame3D global local -> Curve3D local -> Curve3D global
placeIn frame curve = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.placeIn frame)
          (Point3D.placeIn frame)
          (Bounds3D.placeIn frame)
          (compiled curve)
  Curve.unsafe compiledPlaced (VectorCurve3D.placeIn frame (derivative curve))

relativeTo :: Frame3D global local -> Curve3D global -> Curve3D local
relativeTo frame curve = placeIn (Frame3D.inverse frame) curve

findPoint ::
  Tolerance Meters =>
  Point3D space ->
  Curve3D space ->
  Result Curve.IsDegenerateAndCoincidentWithPoint (List Number)
findPoint = Curve.findPoint

intersections ::
  Tolerance Meters =>
  Curve3D space ->
  Curve3D space ->
  Result (Curve.Intersections.Error 3 Meters space) (Maybe Curve.Intersections)
intersections = Curve.intersections
