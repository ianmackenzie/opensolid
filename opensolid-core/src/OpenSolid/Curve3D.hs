module OpenSolid.Curve3D
  ( Curve3D
  , HasDegeneracy (HasDegeneracy)
  , Compiled
  , Segment
  , SearchTree
  , new
  , constant
  , on
  , line
  , lineFrom
  , bezier
  , quadraticBezier
  , cubicBezier
  , hermite
  , derivative
  , secondDerivative
  , derivativeValue
  , derivativeBounds
  , secondDerivativeValue
  , secondDerivativeBounds
  , tangentDirection
  , tangentDirectionBounds
  , curvatureVector
  , startPoint
  , endPoint
  , endpoints
  , point
  , bounds
  , overallBounds
  , reverse
  , arcLengthParameterizationFunction
  , arcLengthParameterization
  , parameterizeByArcLength
  , transformBy
  , placeIn
  , relativeTo
  , findPoint
  , intersections
  , searchTree
  )
where

import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve (Curve, HasSingularity)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve2D (Curve2D)
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.DirectionCurve3D (DirectionCurve3D)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Interval (Interval)
import OpenSolid.Length (Length)
import OpenSolid.Line3D (Line3D)
import OpenSolid.Nondegenerate (IsDegenerate)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Result qualified as Result
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Units (InverseMeters)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorCurve3D (VectorCurve3D)
import OpenSolid.VectorCurve3D qualified as VectorCurve3D

type Curve3D space = Curve 3 Meters space

type Compiled space = Curve.Compiled 3 Meters space

type Segment space = Curve.Segment 3 Meters space

type SearchTree space = Curve.SearchTree 3 Meters space

data HasDegeneracy = HasDegeneracy deriving (Eq, Show)

new :: Compiled space -> VectorCurve3D Meters space -> Curve3D space
new = Curve.new

constant :: Point3D space -> Curve3D space
constant = Curve.constant

on :: Plane3D global -> Curve2D Meters local -> Curve3D global
on plane curve2D = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.placeOn plane)
          (Point2D.placeOn plane)
          (Bounds2D.placeOn plane)
          (Curve2D.compiled curve2D)
  new compiledPlaced (VectorCurve3D.on plane (Curve2D.derivative curve2D))

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

derivative :: Curve3D space -> VectorCurve3D Meters space
derivative = Curve.derivative

compiled :: Curve3D space -> Compiled space
compiled = Curve.compiled

secondDerivative :: Curve3D space -> VectorCurve3D Meters space
secondDerivative = Curve.secondDerivative

{-# INLINE derivativeValue #-}
derivativeValue ::
  Curve3D space ->
  Number ->
  Vector3D Meters space
derivativeValue = Curve.derivativeValue

{-# INLINE derivativeBounds #-}
derivativeBounds ::
  Curve3D space ->
  Interval Unitless ->
  VectorBounds3D Meters space
derivativeBounds = Curve.derivativeBounds

{-# INLINE secondDerivativeValue #-}
secondDerivativeValue ::
  Curve3D space ->
  Number ->
  Vector3D Meters space
secondDerivativeValue = Curve.secondDerivativeValue

{-# INLINE secondDerivativeBounds #-}
secondDerivativeBounds ::
  Curve3D space ->
  Interval Unitless ->
  VectorBounds3D Meters space
secondDerivativeBounds = Curve.secondDerivativeBounds

tangentDirection ::
  Tolerance Meters =>
  Curve3D space ->
  Result IsDegenerate (DirectionCurve3D space)
tangentDirection = Curve.tangentDirection

tangentDirectionBounds :: Curve3D space -> Interval Unitless -> DirectionBounds3D space
tangentDirectionBounds = Curve.tangentDirectionBounds

curvatureVector ::
  Tolerance Meters =>
  Curve3D space ->
  Result HasSingularity (VectorCurve3D InverseMeters space)
curvatureVector curve = Result.map Units.specialize (Curve.curvatureVector_ curve)

startPoint :: Curve3D space -> Point3D space
startPoint = Curve.startPoint

endPoint :: Curve3D space -> Point3D space
endPoint = Curve.endPoint

endpoints :: Curve3D space -> (Point3D space, Point3D space)
endpoints = Curve.endpoints

point :: Curve3D space -> Number -> Point3D space
point = Curve.point

bounds :: Curve3D space -> Interval Unitless -> Bounds3D space
bounds = Curve.bounds

overallBounds :: Curve3D space -> Bounds3D space
overallBounds = Curve.overallBounds

reverse :: Curve3D space -> Curve3D space
reverse = Curve.reverse

arcLengthParameterizationFunction :: Tolerance Meters => Curve3D space -> (Number -> Number, Length)
arcLengthParameterizationFunction = Curve.arcLengthParameterizationFunction

arcLengthParameterization :: Tolerance Meters => Curve3D space -> (Curve1D Unitless, Length)
arcLengthParameterization = Curve.arcLengthParameterization

parameterizeByArcLength :: Tolerance Meters => Curve3D space -> (Curve3D space, Length)
parameterizeByArcLength = Curve.parameterizeByArcLength

transformBy :: Transform3D tag space -> Curve3D space -> Curve3D space
transformBy transform curve = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.transformBy transform)
          (Point3D.transformBy transform)
          (Bounds3D.transformBy transform)
          (compiled curve)
  new compiledTransformed (VectorCurve3D.transformBy transform (derivative curve))

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

findPoint :: Tolerance Meters => Point3D space -> Curve3D space -> List Number
findPoint = Curve.findPoint

intersections ::
  Tolerance Meters =>
  Curve3D space ->
  Curve3D space ->
  Result IsDegenerate (Maybe Curve.Intersections)
intersections = Curve.intersections

searchTree :: Curve3D space -> SearchTree space
searchTree = Curve.searchTree
