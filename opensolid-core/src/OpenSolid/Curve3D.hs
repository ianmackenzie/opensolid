module OpenSolid.Curve3D
  ( Curve3D
  , Compiled
  , Segment
  , new
  , constant
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
  , derivativeAt
  , derivativeRange
  , secondDerivativeAt
  , secondDerivativeRange
  , tangentDirectionRange
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
  , uniformPoint
  , transformBy
  , placeIn
  , relativeTo
  , findPoint
  , IntersectionPoint
  , Intersections
  , intersections
  )
where

import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve (Curve3D)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.CurvePoint3D (CurvePoint3D)
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.Error (IsDegenerate)
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
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorCurve3D (VectorCurve3D)
import OpenSolid.VectorCurve3D qualified as VectorCurve3D

type Compiled space = Curve.Compiled 3 Meters space

type Segment space = Curve.Segment 3 Meters space

new :: Compiled space -> VectorCurve3D Meters space -> Curve3D space
new = Curve.new

constant :: Point3D space -> Curve3D space
constant = Curve.constant

on :: Plane3D space -> Curve2D Meters -> Curve3D space
on = Curve.placeOn

line :: Line3D space -> Curve3D space
line = Curve.line

lineFrom :: Point3D space -> Point3D space -> Curve3D space
lineFrom = Curve.lineFrom

{-| Construct a Bezier curve from its control points. For example,

> Curve3D.bezier (NonEmpty.four p1 p2 p3 p4))

will return a cubic Bezier curve with the given four control points.
-}
bezier :: NonEmpty (Point3D space) -> Curve3D space
bezier = Curve.bezier

-- | Construct a quadratic Bezier curve from the given control points.
quadraticBezier :: Point3D space -> Point3D space -> Point3D space -> Curve3D space
quadraticBezier = Curve.quadraticBezier

-- | Construct a cubic Bezier curve from the given control points.
cubicBezier :: Point3D space -> Point3D space -> Point3D space -> Point3D space -> Curve3D space
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

{-# INLINE derivativeAt #-}
derivativeAt :: Number -> Curve3D space -> Vector3D Meters space
derivativeAt = Curve.derivativeAt

{-# INLINE derivativeRange #-}
derivativeRange :: Interval Unitless -> Curve3D space -> VectorBounds3D Meters space
derivativeRange = Curve.derivativeRange

{-# INLINE secondDerivativeAt #-}
secondDerivativeAt :: Number -> Curve3D space -> Vector3D Meters space
secondDerivativeAt = Curve.secondDerivativeAt

{-# INLINE secondDerivativeRange #-}
secondDerivativeRange :: Interval Unitless -> Curve3D space -> VectorBounds3D Meters space
secondDerivativeRange = Curve.secondDerivativeRange

tangentDirectionRange :: Interval Unitless -> Curve3D space -> DirectionBounds3D space
tangentDirectionRange = Curve.tangentDirectionRange

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

uniformPoint :: Tolerance Meters => Number -> Curve3D space -> Point3D space
uniformPoint = Curve.uniformPoint

transformBy :: Transform3D tag space -> Curve3D space -> Curve3D space
transformBy = Curve.transformBy

placeIn :: Frame3D global local -> Curve3D local -> Curve3D global
placeIn frame curve = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.placeIn frame)
          (Point3D.placeIn frame)
          (Bounds3D.placeIn frame)
          (compiled curve)
  new compiledPlaced (VectorCurve3D.placeIn frame (derivative curve))

relativeTo :: Frame3D global local -> Curve3D global -> Curve3D local
relativeTo frame curve = placeIn (Frame3D.inverse frame) curve

findPoint ::
  Tolerance Meters =>
  Point3D space ->
  Curve3D space ->
  Result IsDegenerate (List (CurvePoint3D space))
findPoint = Curve.findPoint

type IntersectionPoint space = Curve.IntersectionPoint 3 Meters space

type Intersections space = Curve.Intersections 3 Meters space

intersections ::
  Tolerance Meters =>
  Curve3D space ->
  Curve3D space ->
  Result IsDegenerate (Maybe (Intersections space))
intersections = Curve.intersections
