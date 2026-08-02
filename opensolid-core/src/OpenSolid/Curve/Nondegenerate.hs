{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve.Nondegenerate
  ( hasDegenerateStart
  , hasDegenerateEnd
  , pointAt
  , pointOn
  , startPoint
  , endPoint
  , bounds
  , derivative
  , derivativeAt
  , secondDerivativeAt
  , tangentDirectionAt
  , tangentDirectionRange
  , bisectionTree
  , findPoint
  , continuityAt
  , intersections
  )
where

import OpenSolid.Bag qualified as Bag
import OpenSolid.Bisection qualified as Bisection
import OpenSolid.Bounds (Bounds)
import OpenSolid.Continuity (Continuity)
import OpenSolid.Continuity qualified as Continuity
import OpenSolid.Curve (Curve, CurveExists)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.CurvatureVector qualified as Curve.CurvatureVector
import OpenSolid.Curve.Intersections (Intersections)
import {-# SOURCE #-} OpenSolid.Curve.Nondegenerate.Intersections qualified as Curve.Nondegenerate.Intersections
import OpenSolid.Curve.Segment qualified as Curve.Segment
import OpenSolid.Direction (Direction, DirectionExists)
import OpenSolid.Direction qualified as Direction
import OpenSolid.DirectionBounds (DirectionBounds, DirectionBoundsExists)
import OpenSolid.Fuzzy qualified as Fuzzy
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson.Curve qualified as NewtonRaphson.Curve
import OpenSolid.NewtonRaphson.Surface qualified as NewtonRaphson.Surface
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Number qualified as Number
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve.Nondegenerate qualified as VectorCurve.Nondegenerate

hasDegenerateStart ::
  CurveExists dimension units space =>
  Nondegenerate (Curve dimension units space) -> Bool
hasDegenerateStart (Nondegenerate curve) = Curve.hasDegenerateStart curve

hasDegenerateEnd ::
  CurveExists dimension units space =>
  Nondegenerate (Curve dimension units space) -> Bool
hasDegenerateEnd (Nondegenerate curve) = Curve.hasDegenerateEnd curve

{-# INLINE pointAt #-}
pointAt ::
  CurveExists dimension units space =>
  Number ->
  Nondegenerate (Curve dimension units space) ->
  Point dimension units space
pointAt tValue (Nondegenerate curve) = Curve.pointAt tValue curve

pointOn ::
  CurveExists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Number ->
  Point dimension units space
pointOn curve tValue = pointAt tValue curve

startPoint :: Nondegenerate (Curve dimension units space) -> Point dimension units space
startPoint (Nondegenerate curve) = Curve.startPoint curve

endPoint :: Nondegenerate (Curve dimension units space) -> Point dimension units space
endPoint (Nondegenerate curve) = Curve.endPoint curve

bounds ::
  CurveExists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Bounds dimension units space
bounds (Nondegenerate curve) = Curve.bounds curve

derivative ::
  CurveExists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (VectorCurve dimension units space)
derivative (Nondegenerate curve) = Nondegenerate (Curve.derivative curve)

{-# INLINE derivativeAt #-}
derivativeAt ::
  CurveExists dimension units space =>
  Number ->
  Nondegenerate (Curve dimension units space) ->
  Vector dimension units space
derivativeAt tValue (Nondegenerate curve) =
  Curve.derivativeAt tValue curve

{-# INLINE secondDerivativeAt #-}
secondDerivativeAt ::
  CurveExists dimension units space =>
  Number ->
  Nondegenerate (Curve dimension units space) ->
  Vector dimension units space
secondDerivativeAt tValue (Nondegenerate curve) =
  Curve.secondDerivativeAt tValue curve

tangentDirectionAt ::
  (CurveExists dimension units space, DirectionExists dimension space) =>
  Number ->
  Nondegenerate (Curve dimension units space) ->
  Direction dimension space
tangentDirectionAt tValue curve =
  VectorCurve.Nondegenerate.directionAt tValue (derivative curve)

tangentDirectionRange ::
  (CurveExists dimension units space, DirectionBoundsExists dimension space) =>
  Interval Unitless ->
  Nondegenerate (Curve dimension units space) ->
  DirectionBounds dimension space
tangentDirectionRange tRange curve =
  VectorCurve.Nondegenerate.directionRange tRange (derivative curve)

bisectionTree ::
  Nondegenerate (Curve dimension units space) ->
  Curve.BisectionTree dimension units space
bisectionTree = Curve.bisectionTree

data Monotonic = Monotonic deriving (Eq)

findPoint ::
  (CurveExists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Nondegenerate (Curve dimension units space) ->
  List Number
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

isDegenerateAt ::
  (CurveExists dimension units space, Tolerance units) =>
  Number ->
  Nondegenerate (Curve dimension units space) ->
  Bool
isDegenerateAt 0.0 curve = hasDegenerateStart curve
isDegenerateAt 1.0 curve = hasDegenerateEnd curve
isDegenerateAt _ _ = False -- Assume no interior degeneracies

continuityAt ::
  forall dimension units space.
  (CurveExists dimension units space, Tolerance units) =>
  (Number, Number) ->
  (Nondegenerate (Curve dimension units space), Nondegenerate (Curve dimension units space)) ->
  Maybe Continuity
continuityAt (t1, t2) (curve1, curve2)
  | pointAt t1 curve1 ~= pointAt t2 curve2 = do
      let tangent1 = tangentDirectionAt t1 curve1
      let tangent2 = tangentDirectionAt t2 curve2
      if Direction.independent tangent1 tangent2
        then Just Continuity.Crossing
        else do
          let alignment = Number.sign (tangent1 `dot` tangent2)
          if isDegenerateAt t1 curve1 || isDegenerateAt t2 curve2
            then Just (Continuity.Indistinguishable alignment)
            else do
              let firstDerivative1 = derivativeAt t1 curve1
              let firstDerivative2 = derivativeAt t2 curve2
              let secondDerivative1 = secondDerivativeAt t1 curve1
              let secondDerivative2 = secondDerivativeAt t2 curve2
              let l1 = Vector.magnitude firstDerivative1
              let l2 = Vector.magnitude firstDerivative2
              let l = Quantity.erase (min l1 l2)
              let k1_ = Curve.CurvatureVector.value_ firstDerivative1 secondDerivative1
              let k2_ = Curve.CurvatureVector.value_ firstDerivative2 secondDerivative2
              let k = Vector.erase (k1_ - k2_)
              let curvatureError = Vector.unerase @units (k * l * l / 2.0)
              if curvatureError ~= Vector.zero
                then Just (Continuity.Indistinguishable alignment)
                else Just (Continuity.Tangent alignment)
  | otherwise = Nothing

intersections ::
  ( CurveExists dimension units space
  , NewtonRaphson.Surface.Solver dimension units space
  , Tolerance units
  ) =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (Curve dimension units space) ->
  Maybe Intersections
intersections = Curve.Nondegenerate.Intersections.intersections
