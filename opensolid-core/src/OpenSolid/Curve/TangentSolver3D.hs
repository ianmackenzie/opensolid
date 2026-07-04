{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve.TangentSolver3D (solver) where

import OpenSolid.Curve (Curve3D)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.IntersectionPoint (IntersectionPoint)
import OpenSolid.Curve.Segment qualified as Curve.Segment
import OpenSolid.Curve.TangentSolver qualified as TangentSolver
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Point3D (Point3D)
  , Vector3D (Vector3D)
  , VectorBounds3D (VectorBounds3D)
  )
import OpenSolid.Units qualified as Units
import OpenSolid.UvPoint (pattern UvPoint)
import OpenSolid.Vector2D (Vector2D (Vector2D))

solver :: Curve.Solver 3 Meters space
solver = Curve.Solver resolve solve

data Orientation = XY | YX | YZ | ZY | ZX | XZ

resolve ::
  Tolerance Meters =>
  (Curve.Segment 3 Meters space, Curve.Segment 3 Meters space) ->
  Fuzzy (Maybe Orientation)
resolve (segmentA, segmentB) =
  if TangentSolver.areDistinctOrCrossing segmentA segmentB
    then Resolved Nothing
    else do
      let VectorBounds3D dxdtA dydtA dzdtA = Curve.Segment.derivativeRange segmentA
      let VectorBounds3D dxdtB dydtB dzdtB = Curve.Segment.derivativeRange segmentB
      let monotonicX = Interval.isResolved dxdtA && Interval.isResolved dxdtB
      let monotonicY = Interval.isResolved dydtA && Interval.isResolved dydtB
      let monotonicZ = Interval.isResolved dzdtA && Interval.isResolved dzdtB
      let isResolved secondDerivativeRange =
            Interval.isResolved (secondDerivativeRange segmentA - secondDerivativeRange segmentB)
      if
        | monotonicX && isResolved d2ydx2Range -> Resolved (Just XY)
        | monotonicX && isResolved d2zdx2Range -> Resolved (Just XZ)
        | monotonicY && isResolved d2zdy2Range -> Resolved (Just YZ)
        | monotonicY && isResolved d2xdy2Range -> Resolved (Just YX)
        | monotonicZ && isResolved d2xdz2Range -> Resolved (Just ZX)
        | monotonicZ && isResolved d2ydz2Range -> Resolved (Just ZY)
        | otherwise -> Unresolved

d2ydx2Range :: Curve.Segment 3 Meters space -> Interval (Unitless ?/? Meters)
d2ydx2Range segment = do
  let VectorBounds3D dxdt dydt _ = Curve.Segment.derivativeRange segment
  let VectorBounds3D d2xdt2 d2ydt2 _ = Curve.Segment.secondDerivativeRange segment
  TangentSolver.secondDerivativeRange dxdt dydt d2xdt2 d2ydt2

d2xdy2Range :: Curve.Segment 3 Meters space -> Interval (Unitless ?/? Meters)
d2xdy2Range segment = do
  let VectorBounds3D dxdt dydt _ = Curve.Segment.derivativeRange segment
  let VectorBounds3D d2xdt2 d2ydt2 _ = Curve.Segment.secondDerivativeRange segment
  TangentSolver.secondDerivativeRange dydt dxdt d2ydt2 d2xdt2

d2zdy2Range :: Curve.Segment 3 Meters space -> Interval (Unitless ?/? Meters)
d2zdy2Range segment = do
  let VectorBounds3D _ dydt dzdt = Curve.Segment.derivativeRange segment
  let VectorBounds3D _ d2ydt2 d2zdt2 = Curve.Segment.secondDerivativeRange segment
  TangentSolver.secondDerivativeRange dydt dzdt d2ydt2 d2zdt2

d2ydz2Range :: Curve.Segment 3 Meters space -> Interval (Unitless ?/? Meters)
d2ydz2Range segment = do
  let VectorBounds3D _ dydt dzdt = Curve.Segment.derivativeRange segment
  let VectorBounds3D _ d2ydt2 d2zdt2 = Curve.Segment.secondDerivativeRange segment
  TangentSolver.secondDerivativeRange dzdt dydt d2zdt2 d2ydt2

d2xdz2Range :: Curve.Segment 3 Meters space -> Interval (Unitless ?/? Meters)
d2xdz2Range segment = do
  let VectorBounds3D dxdt _ dzdt = Curve.Segment.derivativeRange segment
  let VectorBounds3D d2xdt2 _ d2zdt2 = Curve.Segment.secondDerivativeRange segment
  TangentSolver.secondDerivativeRange dzdt dxdt d2zdt2 d2xdt2

d2zdx2Range :: Curve.Segment 3 Meters space -> Interval (Unitless ?/? Meters)
d2zdx2Range segment = do
  let VectorBounds3D dxdt _ dzdt = Curve.Segment.derivativeRange segment
  let VectorBounds3D d2xdt2 _ d2zdt2 = Curve.Segment.secondDerivativeRange segment
  TangentSolver.secondDerivativeRange dxdt dzdt d2xdt2 d2zdt2

solve ::
  Tolerance Meters =>
  Orientation ->
  Nondegenerate (Curve3D space) ->
  Nondegenerate (Curve3D space) ->
  (Interval Unitless, Interval Unitless) ->
  (Curve.Segment 3 Meters space, Curve.Segment 3 Meters space) ->
  Fuzzy (Maybe (IntersectionPoint 3 Meters space))
solve orientation nondegenerateA nondegenerateB (tRangeA, tRangeB) (segmentA, segmentB) = do
  if TangentSolver.areDistinctOrCrossing segmentA segmentB
    then Resolved Nothing
    else do
      let Nondegenerate curveA = nondegenerateA
      let Nondegenerate curveB = nondegenerateB
      let scale = TangentSolver.lengthScale curveA curveB
      let evaluate (UvPoint tA tB) = do
            let Point3D xA yA zA = Curve.point curveA tA
            let Point3D xB yB zB = Curve.point curveB tB
            let Vector3D x'A y'A z'A = Curve.derivativeValue curveA tA
            let Vector3D x'B y'B z'B = Curve.derivativeValue curveB tB
            let Vector3D x''A y''A z''A = Curve.secondDerivativeValue curveA tA
            let Vector3D x''B y''B z''B = Curve.secondDerivativeValue curveB tB
            let crossProductXY = Units.simplify do (x'A ?*? y'B - y'A ?*? x'B) ?/? scale
            let crossProductXY'A = Units.simplify do (x''A ?*? y'B - y''A ?*? x'B) ?/? scale
            let crossProductXY'B = Units.simplify do (x'A ?*? y''B - y'A ?*? x''B) ?/? scale
            let crossProductXZ = Units.simplify do (x'A ?*? z'B - z'A ?*? x'B) ?/? scale
            let crossProductXZ'A = Units.simplify do (x''A ?*? z'B - z''A ?*? x'B) ?/? scale
            let crossProductXZ'B = Units.simplify do (x'A ?*? z''B - z'A ?*? x''B) ?/? scale
            let crossProductYZ = Units.simplify do (y'A ?*? z'B - z'A ?*? y'B) ?/? scale
            let crossProductYZ'A = Units.simplify do (y''A ?*? z'B - z''A ?*? y'B) ?/? scale
            let crossProductYZ'B = Units.simplify do (y'A ?*? z''B - z'A ?*? y''B) ?/? scale
            case orientation of
              XY -> do
                let f = Vector2D (xB - xA) crossProductXY
                let fA = Vector2D -x'A crossProductXY'A
                let fB = Vector2D x'B crossProductXY'B
                (# f, fA, fB #)
              YX -> do
                let f = Vector2D (yB - yA) crossProductXY
                let fA = Vector2D -y'A crossProductXY'A
                let fB = Vector2D y'B crossProductXY'B
                (# f, fA, fB #)
              YZ -> do
                let f = Vector2D (yB - yA) crossProductYZ
                let fA = Vector2D -y'A crossProductYZ'A
                let fB = Vector2D y'B crossProductYZ'B
                (# f, fA, fB #)
              ZY -> do
                let f = Vector2D (zB - zA) crossProductYZ
                let fA = Vector2D -z'A crossProductYZ'A
                let fB = Vector2D z'B crossProductYZ'B
                (# f, fA, fB #)
              XZ -> do
                let f = Vector2D (xB - xA) crossProductXZ
                let fA = Vector2D -x'A crossProductXZ'A
                let fB = Vector2D x'B crossProductXZ'B
                (# f, fA, fB #)
              ZX -> do
                let f = Vector2D (zB - zA) crossProductXZ
                let fA = Vector2D -z'A crossProductXZ'A
                let fB = Vector2D z'B crossProductXZ'B
                (# f, fA, fB #)
      TangentSolver.solve nondegenerateA nondegenerateB tRangeA tRangeB evaluate
