module OpenSolid.Curve
  ( Curve
  , Exists
  , Segment
  , SearchTree
  , IsPoint (IsPoint)
  , derivative
  , bounds
  , evaluate
  , evaluateBounds
  , startPoint
  , endPoint
  , secondDerivative
  , isPoint
  , tangentDirection
  , curvatureVector_
  , unsafeCurvatureVector_
  , unsafeCurvatureVectorImpl_
  , findPoint
  , searchTree
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Curve.Search qualified as Search
import OpenSolid.Curve.Segment (Segment)
import {-# SOURCE #-} OpenSolid.Curve1D.WithNoInteriorZeros qualified as Curve1D.WithNoInteriorZeros
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import {-# SOURCE #-} OpenSolid.Curve3D (Curve3D)
import {-# SOURCE #-} OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Interval (Interval)
import OpenSolid.Point (Point)
import OpenSolid.Point qualified as Point
import OpenSolid.Prelude
import OpenSolid.Result qualified as Result
import OpenSolid.Units qualified as Units
import OpenSolid.Vector qualified as Vector
import OpenSolid.VectorBounds qualified as VectorBounds
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve qualified as VectorCurve

type family Curve dimension units space = curve | curve -> dimension units space where
  Curve 2 units space = Curve2D units space
  Curve 3 Meters space = Curve3D space

data IsPoint = IsPoint deriving (Eq, Show)

type SearchTree dimension units space =
  Search.Tree dimension units space

class
  ( Point.Exists dimension units space
  , Bounds.Exists dimension units space
  , Vector.Exists dimension units space
  , Vector.Exists dimension (Unitless ?/? units) space
  , VectorBounds.Exists dimension units space
  , VectorBounds.Exists dimension (Unitless ?/? units) space
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
  ) =>
  Exists dimension units space
  where
  derivative :: Curve dimension units space -> VectorCurve dimension units space
  bounds :: Curve dimension units space -> Bounds dimension units space
  evaluate :: Curve dimension units space -> Number -> Point dimension units space
  evaluateBounds :: Curve dimension units space -> Interval Unitless -> Bounds dimension units space
  unsafeCurvatureVector_ :: Curve dimension units space -> VectorCurve dimension (Unitless ?/? units) space

instance Exists 2 units space where
  derivative = Curve2D.derivative
  bounds = Curve2D.bounds
  evaluate = Curve2D.evaluate
  evaluateBounds = Curve2D.evaluateBounds
  unsafeCurvatureVector_ = Curve2D.unsafeCurvatureVector_

instance Exists 3 Meters space where
  derivative = Curve3D.derivative
  bounds = Curve3D.bounds
  evaluate = Curve3D.evaluate
  evaluateBounds = Curve3D.evaluateBounds
  unsafeCurvatureVector_ = Units.unspecialize . Curve3D.unsafeCurvatureVector

secondDerivative ::
  Exists dimension units space =>
  Curve dimension units space ->
  VectorCurve dimension units space
secondDerivative = VectorCurve.derivative . derivative

isPoint :: (Exists dimension units space, Tolerance units) => Curve dimension units space -> Bool
isPoint curve = VectorCurve.isZero (derivative curve)

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
  let dsdt = VectorCurve.unsafeMagnitude firstDerivative
  let tangent = VectorCurve.unsafeNormalize firstDerivative
  VectorCurve.unerase (VectorCurve.derivative tangent / Curve1D.WithNoInteriorZeros.erase dsdt)

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

findPoint ::
  (Exists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Curve dimension units space ->
  Result IsPoint (List Number)
findPoint point curve = Result.map (Search.findPoint point) (searchTree curve)

searchTree ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Result IsPoint (SearchTree dimension units space)
searchTree = Search.tree
