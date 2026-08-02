{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve.Nondegenerate
  ( pointAt
  , pointOn
  , curvePointAt
  , curvePointOn
  , bounds
  , derivative
  , derivativeAt
  , secondDerivativeAt
  , tangentDirectionAt
  , bisectionTree
  , findPoint
  , intersections
  )
where

import OpenSolid.Bag qualified as Bag
import OpenSolid.Bisection qualified as Bisection
import OpenSolid.Bounds (Bounds)
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import {-# SOURCE #-} OpenSolid.Curve.Intersections (Intersections)
import {-# SOURCE #-} OpenSolid.Curve.Nondegenerate.Intersections qualified as Curve.Nondegenerate.Intersections
import OpenSolid.Curve.Nonzero qualified as Curve.Nonzero
import OpenSolid.Curve.Segment qualified as Curve.Segment
import OpenSolid.CurveLocation qualified as CurveLocation
import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.Fuzzy qualified as Fuzzy
import OpenSolid.Internal.CurvePoint (CurvePoint (..))
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson.Curve qualified as NewtonRaphson.Curve
import OpenSolid.NewtonRaphson.Surface qualified as NewtonRaphson.Surface
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Nondegenerate qualified as Nondegenerate
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.Vector (Vector)
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve.Nondegenerate qualified as VectorCurve.Nondegenerate

{-# INLINE pointAt #-}
pointAt ::
  Curve.Exists dimension units space =>
  Number ->
  Nondegenerate (Curve dimension units space) ->
  Point dimension units space
pointAt tValue (Nondegenerate curve) = Curve.pointAt tValue curve

pointOn ::
  Curve.Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Number ->
  Point dimension units space
pointOn curve tValue = pointAt tValue curve

curvePointAt ::
  Curve.Exists dimension units space =>
  Number ->
  Nondegenerate (Curve dimension units space) ->
  CurvePoint dimension units space
curvePointAt tValue curve =
  recursive \result ->
    CurvePoint
      { location = CurveLocation.fromParameterValue tValue
      , point = pointAt tValue curve
      , derivative = derivativeAt tValue curve
      , tangentDirection = tangentDirectionAt tValue curve
      , curvatureVector_ =
          result
            & Nondegenerate.field \_ ->
              Curve.Nonzero.curvatureVectorAt_ tValue (Nondegenerate.interior curve)
      }

curvePointOn ::
  Curve.Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Number ->
  CurvePoint dimension units space
curvePointOn curve tValue = curvePointAt tValue curve

bounds ::
  Curve.Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Bounds dimension units space
bounds (Nondegenerate curve) = Curve.bounds curve

derivative ::
  Curve.Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (VectorCurve dimension units space)
derivative (Nondegenerate curve) = Nondegenerate (Curve.derivative curve)

{-# INLINE derivativeAt #-}
derivativeAt ::
  Curve.Exists dimension units space =>
  Number ->
  Nondegenerate (Curve dimension units space) ->
  Vector dimension units space
derivativeAt tValue (Nondegenerate curve) =
  Curve.derivativeAt tValue curve

{-# INLINE secondDerivativeAt #-}
secondDerivativeAt ::
  Curve.Exists dimension units space =>
  Number ->
  Nondegenerate (Curve dimension units space) ->
  Vector dimension units space
secondDerivativeAt tValue (Nondegenerate curve) =
  Curve.secondDerivativeAt tValue curve

tangentDirectionAt ::
  (Curve.Exists dimension units space, Direction.Exists dimension space) =>
  Number ->
  Nondegenerate (Curve dimension units space) ->
  Direction dimension space
tangentDirectionAt tValue curve =
  VectorCurve.Nondegenerate.directionAt tValue (derivative curve)

bisectionTree ::
  Nondegenerate (Curve dimension units space) ->
  Curve.BisectionTree dimension units space
bisectionTree = Curve.bisectionTree

data Monotonic = Monotonic deriving (Eq)

findPoint ::
  (Curve.Exists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Nondegenerate (Curve dimension units space) ->
  List (CurvePoint dimension units space)
findPoint givenPoint givenCurve = do
  let endpointSolutions = [t | t <- [0.0, 1.0], pointAt t givenCurve ~= givenPoint]
  let endpointSolutionSet = Bag.pack Interval.constant endpointSolutions
  let isDistant segment = not (givenPoint `intersects` Curve.Segment.range segment)
  let resolvedMonotonicity _ segment
        | isDistant segment = Resolved Nothing
        | Curve.Segment.isMonotonic segment = Resolved (Just Monotonic)
        | Curve.Segment.isDegenerate segment = Resolved (Just Monotonic)
        | otherwise = Unresolved
  let evaluate tValue =
        (# pointAt tValue givenCurve - givenPoint, derivativeAt tValue givenCurve #)
  let resolvedSolution Monotonic tRange segment
        | isDistant segment = Resolved Nothing
        | otherwise = Fuzzy.map Just (NewtonRaphson.Curve.solveIn tRange evaluate)
  let clusters =
        Curve.bisectionTree givenCurve
          & Bisection.clusters endpointSolutionSet resolvedMonotonicity
  let interiorSolutions = List.filterMap (Bisection.find resolvedSolution) clusters
  List.sort (endpointSolutions <> interiorSolutions)
    & List.map (curvePointOn givenCurve)

intersections ::
  ( Curve.Exists dimension units space
  , NewtonRaphson.Surface.Solver dimension units space
  , Tolerance units
  ) =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (Curve dimension units space) ->
  Maybe (Intersections dimension units space)
intersections = Curve.Nondegenerate.Intersections.intersections
