module OpenSolid.Curve
  ( Curve
  , Exists
  , Segment
  , SearchTree
  , IsPoint (IsPoint)
  , HasSingularity (HasSingularity)
  , derivative
  , overallBounds
  , evaluate
  , bounds
  , startPoint
  , endPoint
  , secondDerivative
  , derivativeValue
  , derivativeBounds
  , secondDerivativeValue
  , secondDerivativeBounds
  , isPoint
  , nondegenerate
  , nonzero
  , tangentDirection
  , curvatureVector_
  , unsafeCurvatureVector_
  , unsafeCurvatureVectorImpl_
  , findPoint
  , searchTree
  , Intersections (IntersectionPoints, OverlappingSegments)
  , IntersectionPoint
  , intersections
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Curve.IntersectionPoint (IntersectionPoint)
import {-# SOURCE #-} OpenSolid.Curve.Intersections (Intersections)
import {-# SOURCE #-} OpenSolid.Curve.Intersections qualified as Intersections
import OpenSolid.Curve.Search qualified as Search
import OpenSolid.Curve.Segment (Segment)
import {-# SOURCE #-} OpenSolid.Curve1D.Nondegenerate qualified as Curve1D.Nondegenerate
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import {-# SOURCE #-} OpenSolid.Curve3D (Curve3D)
import {-# SOURCE #-} OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Interval (Interval)
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Point (Point)
import OpenSolid.Point qualified as Point
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector
import OpenSolid.VectorBounds (VectorBounds)
import OpenSolid.VectorBounds qualified as VectorBounds
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve qualified as VectorCurve
import OpenSolid.VectorCurve.Nondegenerate qualified as VectorCurve.Nondegenerate

type family Curve dimension units space = curve | curve -> dimension units space where
  Curve 2 units space = Curve2D units space
  Curve 3 Meters space = Curve3D space

data IsPoint = IsPoint deriving (Eq, Show)

data HasSingularity = HasSingularity deriving (Eq, Show)

type SearchTree dimension units space =
  Search.Tree dimension units space

class
  ( Point.Exists dimension units space
  , Bounds.Exists dimension units space
  , Vector.Exists dimension units space
  , Vector.Exists dimension (Unitless ?/? units) space
  , VectorBounds.Exists dimension units space
  , VectorBounds.Exists dimension (Unitless ?/? units) space
  , DirectionBounds.Exists dimension space
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
  , Intersects
      (Point dimension units space)
      (Curve dimension units space)
      (Tolerance units)
  , Intersects
      (Curve dimension units space)
      (Point dimension units space)
      (Tolerance units)
  ) =>
  Exists dimension units space
  where
  derivative :: Curve dimension units space -> VectorCurve dimension units space
  overallBounds :: Curve dimension units space -> Bounds dimension units space
  evaluate :: Curve dimension units space -> Number -> Point dimension units space
  bounds :: Curve dimension units space -> Interval Unitless -> Bounds dimension units space
  unsafeCurvatureVector_ :: Curve dimension units space -> VectorCurve dimension (Unitless ?/? units) space

instance Exists 2 units space where
  derivative = Curve2D.derivative
  overallBounds = Curve2D.overallBounds
  evaluate = Curve2D.evaluate
  bounds = Curve2D.bounds
  unsafeCurvatureVector_ = Curve2D.unsafeCurvatureVector_

instance Exists 3 Meters space where
  derivative = Curve3D.derivative
  overallBounds = Curve3D.overallBounds
  evaluate = Curve3D.evaluate
  bounds = Curve3D.bounds
  unsafeCurvatureVector_ = Units.unspecialize . Curve3D.unsafeCurvatureVector

secondDerivative ::
  Exists dimension units space =>
  Curve dimension units space ->
  VectorCurve dimension units space
secondDerivative = VectorCurve.derivative . derivative

isPoint :: (Exists dimension units space, Tolerance units) => Curve dimension units space -> Bool
isPoint curve = VectorCurve.isZero (derivative curve)

nondegenerate ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Result IsPoint (Nondegenerate (Curve dimension units space))
nondegenerate curve =
  if VectorCurve.isZero (derivative curve) then Error IsPoint else Ok (Nondegenerate curve)

nonzero ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Result HasSingularity (Nonzero (Curve dimension units space))
nonzero curve =
  if derivativeValue curve 0.0 ~= Vector.zero || derivativeValue curve 1.0 ~= Vector.zero
    then Error HasSingularity
    else Ok (Nonzero curve)

tangentDirection ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Result IsPoint (DirectionCurve dimension space)
tangentDirection curve =
  case VectorCurve.direction (derivative curve) of
    Ok directionCurve -> Ok directionCurve
    Error VectorCurve.IsZero -> Error IsPoint

curvatureVector_ ::
  ( Exists dimension units space
  , VectorCurve.Exists dimension (Unitless ?/? units) space
  , Tolerance units
  ) =>
  Curve dimension units space ->
  Result IsPoint (VectorCurve dimension (Unitless ?/? units) space)
curvatureVector_ curve =
  if isPoint curve then Error IsPoint else Ok (unsafeCurvatureVector_ curve)

unsafeCurvatureVectorImpl_ ::
  (Exists dimension units space, VectorCurve.Exists dimension (Unitless ?/? units) space) =>
  VectorCurve dimension units space ->
  VectorCurve dimension (Unitless ?/? units) space
unsafeCurvatureVectorImpl_ firstDerivative = do
  let dsdt = VectorCurve.Nondegenerate.magnitude (Nondegenerate firstDerivative)
  let tangent = firstDerivative / dsdt
  VectorCurve.unerase (VectorCurve.derivative tangent / Curve1D.Nondegenerate.erase dsdt)

startPoint ::
  Exists dimension units space =>
  Curve dimension units space ->
  Point dimension units space
startPoint curve = evaluate curve 0.0

endPoint ::
  Exists dimension units space =>
  Curve dimension units space ->
  Point dimension units space
endPoint curve = evaluate curve 1.0

derivativeValue ::
  Exists dimension units space =>
  Curve dimension units space ->
  Number ->
  Vector dimension units space
derivativeValue curve tValue = VectorCurve.evaluate (derivative curve) tValue

derivativeBounds ::
  Exists dimension units space =>
  Curve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
derivativeBounds curve tBounds = VectorCurve.bounds (derivative curve) tBounds

secondDerivativeValue ::
  Exists dimension units space =>
  Curve dimension units space ->
  Number ->
  Vector dimension units space
secondDerivativeValue curve tValue = VectorCurve.evaluate (secondDerivative curve) tValue

secondDerivativeBounds ::
  Exists dimension units space =>
  Curve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
secondDerivativeBounds curve tBounds = VectorCurve.bounds (secondDerivative curve) tBounds

findPoint ::
  (Exists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Curve dimension units space ->
  List Number
findPoint point curve = Search.findPoint point curve (searchTree curve)

searchTree ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  SearchTree dimension units space
searchTree = Search.tree

intersections ::
  ( Exists dimension units space
  , NewtonRaphson.Surface dimension units space
  , Tolerance units
  ) =>
  Curve dimension units space ->
  Curve dimension units space ->
  Result IsPoint (Maybe Intersections)
intersections = Intersections.intersections
