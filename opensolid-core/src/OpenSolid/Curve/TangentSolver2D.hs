{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve.TangentSolver2D (solver) where

import OpenSolid.Curve (Curve2D)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.IntersectionPoint (IntersectionPoint)
import OpenSolid.Curve.Segment qualified as Curve.Segment
import OpenSolid.Curve.TangentSolver qualified as TangentSolver
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.UvPoint (pattern UvPoint)
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.VectorBounds2D (VectorBounds2D (VectorBounds2D))

solver :: Curve.Solver 2 units Void
solver = Curve.Solver resolve solve

data Orientation = X | Y

resolve ::
  Tolerance units =>
  (Curve.Segment 2 units Void, Curve.Segment 2 units Void) ->
  Fuzzy (Maybe Orientation)
resolve (segmentA, segmentB) =
  if TangentSolver.areDistinctOrCrossing segmentA segmentB
    then Resolved Nothing
    else do
      let VectorBounds2D dxdtA dydtA = Curve.Segment.derivativeRange segmentA
      let VectorBounds2D dxdtB dydtB = Curve.Segment.derivativeRange segmentB
      let monotonicX = Interval.isResolved dxdtA && Interval.isResolved dxdtB
      let monotonicY = Interval.isResolved dydtA && Interval.isResolved dydtB
      let isResolved secondDerivativeRange =
            Interval.isResolved (secondDerivativeRange segmentA - secondDerivativeRange segmentB)
      if
        | monotonicX && isResolved d2ydx2Range -> Resolved (Just X)
        | monotonicY && isResolved d2xdy2Range -> Resolved (Just Y)
        | otherwise -> Unresolved

d2ydx2Range :: Curve.Segment 2 units Void -> Interval (Unitless ?/? units)
d2ydx2Range segment = do
  let VectorBounds2D dxdt dydt = Curve.Segment.derivativeRange segment
  let VectorBounds2D d2xdt2 d2ydt2 = Curve.Segment.secondDerivativeRange segment
  TangentSolver.secondDerivativeRange dxdt dydt d2xdt2 d2ydt2

d2xdy2Range :: Curve.Segment 2 units Void -> Interval (Unitless ?/? units)
d2xdy2Range segment = do
  let VectorBounds2D dxdt dydt = Curve.Segment.derivativeRange segment
  let VectorBounds2D d2xdt2 d2ydt2 = Curve.Segment.secondDerivativeRange segment
  TangentSolver.secondDerivativeRange dydt dxdt d2ydt2 d2xdt2

solve ::
  Tolerance units =>
  Orientation ->
  Nondegenerate (Curve2D units) ->
  Nondegenerate (Curve2D units) ->
  (Interval Unitless, Interval Unitless) ->
  (Curve.Segment 2 units Void, Curve.Segment 2 units Void) ->
  Fuzzy (Maybe (IntersectionPoint 2 units Void))
solve orientation nondegenerateA nondegenerateB (tRangeA, tRangeB) (segmentA, segmentB) =
  if TangentSolver.areDistinctOrCrossing segmentA segmentB
    then Resolved Nothing
    else do
      let Nondegenerate curveA = nondegenerateA
      let Nondegenerate curveB = nondegenerateB
      let scale = TangentSolver.lengthScale curveA curveB
      let evaluate (UvPoint tA tB) = do
            let Point2D xA yA = Curve.point curveA tA
            let Point2D xB yB = Curve.point curveB tB
            let Vector2D x'A y'A = Curve.derivativeValue curveA tA
            let Vector2D x'B y'B = Curve.derivativeValue curveB tB
            let Vector2D x''A y''A = Curve.secondDerivativeValue curveA tA
            let Vector2D x''B y''B = Curve.secondDerivativeValue curveB tB
            let crossProduct = Units.simplify do (x'A ?*? y'B - y'A ?*? x'B) ?/? scale
            let crossProduct'A = Units.simplify do (x''A ?*? y'B - y''A ?*? x'B) ?/? scale
            let crossProduct'B = Units.simplify do (x'A ?*? y''B - y'A ?*? x''B) ?/? scale
            case orientation of
              X -> do
                let f = Vector2D (xB - xA) crossProduct
                let fA = Vector2D -x'A crossProduct'A
                let fB = Vector2D x'B crossProduct'B
                (# f, fA, fB #)
              Y -> do
                let f = Vector2D (yB - yA) crossProduct
                let fA = Vector2D -y'A crossProduct'A
                let fB = Vector2D y'B crossProduct'B
                (# f, fA, fB #)
      TangentSolver.solve nondegenerateA nondegenerateB tRangeA tRangeB evaluate
