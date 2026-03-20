{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve
  ( Curve
  , Exists
  , Segment
  , SearchTree
  , HasSingularity (HasSingularity)
  , derivative
  , overallBounds
  , point
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
import {-# SOURCE #-} OpenSolid.Curve.Nonzero qualified as Curve.Nonzero
import OpenSolid.Curve.Search qualified as Curve.Search
import OpenSolid.Curve.Segment (Segment)
import OpenSolid.Curve.Segment qualified as Curve.Segment
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import {-# SOURCE #-} OpenSolid.Curve3D (Curve3D)
import {-# SOURCE #-} OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.Nondegenerate (IsDegenerate (IsDegenerate), Nondegenerate (Nondegenerate))
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Number qualified as Number
import OpenSolid.Pair qualified as Pair
import OpenSolid.Point (Point)
import OpenSolid.Point qualified as Point
import OpenSolid.Prelude
import OpenSolid.Result qualified as Result
import OpenSolid.Search qualified as Search
import OpenSolid.Search.Domain qualified as Search.Domain
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector
import OpenSolid.VectorBounds (VectorBounds)
import OpenSolid.VectorBounds qualified as VectorBounds
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve qualified as VectorCurve

type family Curve dimension units space = curve | curve -> dimension units space where
  Curve 2 units space = Curve2D units space
  Curve 3 Meters space = Curve3D space

data HasSingularity = HasSingularity deriving (Eq, Show)

type SearchTree dimension units space =
  Curve.Search.Tree dimension units space

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
  point :: Curve dimension units space -> Number -> Point dimension units space
  bounds :: Curve dimension units space -> Interval Unitless -> Bounds dimension units space
  searchTree :: Curve dimension units space -> SearchTree dimension units space

instance Exists 2 units space where
  derivative = Curve2D.derivative
  overallBounds = Curve2D.overallBounds
  point = Curve2D.point
  bounds = Curve2D.bounds
  searchTree = Curve2D.searchTree

instance Exists 3 Meters space where
  derivative = Curve3D.derivative
  overallBounds = Curve3D.overallBounds
  point = Curve3D.point
  bounds = Curve3D.bounds
  searchTree = Curve3D.searchTree

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
  Result IsDegenerate (Nondegenerate (Curve dimension units space))
nondegenerate curve =
  if VectorCurve.isZero (derivative curve) then Error IsDegenerate else Ok (Nondegenerate curve)

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
  Result IsDegenerate (DirectionCurve dimension space)
tangentDirection curve = VectorCurve.direction (derivative curve)

curvatureVector_ ::
  ( Exists dimension units space
  , VectorCurve.Exists dimension (Unitless ?/? units) space
  , Tolerance units
  ) =>
  Curve dimension units space ->
  Result HasSingularity (VectorCurve dimension (Unitless ?/? units) space)
curvatureVector_ curve = Result.map Curve.Nonzero.curvatureVector_ (nonzero curve)

startPoint ::
  Exists dimension units space =>
  Curve dimension units space ->
  Point dimension units space
startPoint curve = point curve 0.0

endPoint ::
  Exists dimension units space =>
  Curve dimension units space ->
  Point dimension units space
endPoint curve = point curve 1.0

derivativeValue ::
  Exists dimension units space =>
  Curve dimension units space ->
  Number ->
  Vector dimension units space
derivativeValue curve tValue = VectorCurve.value (derivative curve) tValue

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
secondDerivativeValue curve tValue = VectorCurve.value (secondDerivative curve) tValue

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
findPoint givenPoint curve = do
  let evaluate tValue = (# point curve tValue - givenPoint, derivativeValue curve tValue #)
  let isSolution tValue = point curve tValue ~= givenPoint
  let isDegenerate tValue = derivativeValue curve tValue ~= Vector.zero
  let endpointSolutions = List.filter isSolution [0.0, 1.0]
  let solveMonotonic tBounds = do
        let tMid = Interval.midpoint tBounds
        let tSolution = NewtonRaphson.curve evaluate tMid
        if Search.isInterior tSolution tBounds && isSolution tSolution
          then Resolved (Just tSolution)
          else Unresolved
  let interiorSolution tBounds segment
        | not (givenPoint `intersects` Curve.Segment.bounds segment) = Resolved Nothing
        | otherwise = do
            let isMonotonic = Curve.Segment.monotonic segment
            let isSmall = Search.Domain.isSmall tBounds
            let endpointSolution = List.find (Number.includedIn tBounds) endpointSolutions
            let hasEndpointSolution = endpointSolution /= Nothing
            if
              | isMonotonic && hasEndpointSolution -> Resolved Nothing
              | isSmall, Just tValue <- endpointSolution, isDegenerate tValue -> Resolved Nothing
              | isMonotonic -> solveMonotonic tBounds
              | otherwise -> Unresolved
  let isDuplicate (tBounds1, _) (tBounds2, _) = Search.Domain.overlapping tBounds1 tBounds2
  let interiorSolutions =
        Search.exclusive interiorSolution isDuplicate (searchTree curve)
          & List.map Pair.second
  List.sort (endpointSolutions <> interiorSolutions)

intersections ::
  ( Exists dimension units space
  , NewtonRaphson.Surface dimension units space
  , Tolerance units
  ) =>
  Curve dimension units space ->
  Curve dimension units space ->
  Result IsDegenerate (Maybe Intersections)
intersections = Intersections.intersections
