{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.NewtonRaphson.Curve
  ( Function
  , Solver
  , solveFrom
  , solveIn
  )
where

import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Vector (Vector)
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D

type Function dimension units space =
  Number -> (# Vector dimension units space, Vector dimension units space #)

class Solver dimension units space where
  solveFrom :: Number -> Function dimension units space -> Fuzzy Number

solveIn ::
  Solver dimension units space =>
  Interval Unitless ->
  Function dimension units space ->
  Fuzzy Number
solveIn tRange evaluate = do
  tSolution <- solveFrom (Interval.midpoint tRange) evaluate
  if Interval.inclusion tSolution tRange >= 0.0
    then Resolved tSolution
    else Unresolved

validateSolution :: Number -> Number -> Fuzzy Number
validateSolution t1 tStep =
  if Number.abs tStep <= Tolerance.unitless || Number.isNaN tStep then Resolved t1 else Unresolved

instance Solver 1 units Void where
  solveFrom t1 evaluate = do
    let (# v1, dv1 #) = evaluate t1
    solve1D evaluate t1 v1 dv1

solve1D :: Function 1 units Void -> Number -> Quantity units -> Quantity units -> Fuzzy Number
solve1D evaluate t1 v1 dv1 = do
  let tStep = v1 / dv1
  let t2 = t1 - tStep
  let (# v2, dv2 #) = evaluate t2
  if Quantity.abs v2 < 0.5 * Quantity.abs v1
    then solve1D evaluate t2 v2 dv2
    else validateSolution t1 tStep

instance Solver 2 units Void where
  solveFrom t1 evaluate = do
    let (# v1, d1 #) = evaluate t1
    solve2D evaluate t1 v1 d1

solve2D :: Function 2 units Void -> Number -> Vector2D units -> Vector2D units -> Fuzzy Number
solve2D evaluate t1 v1 d1 = do
  let tStep = (v1 `dot_` d1) / (d1 `dot_` d1)
  let t2 = t1 - tStep
  let (# v2, d2 #) = evaluate t2
  if Vector2D.squaredMagnitude_ v2 < 0.25 * Vector2D.squaredMagnitude_ v1
    then solve2D evaluate t2 v2 d2
    else validateSolution t1 tStep

instance Solver 3 units space where
  solveFrom t1 evaluate = do
    let (# v1, d1 #) = evaluate t1
    solve3D evaluate t1 v1 d1

solve3D ::
  Function 3 units space ->
  Number ->
  Vector3D units space ->
  Vector3D units space ->
  Fuzzy Number
solve3D evaluate t1 v1 d1 = do
  let tStep = (v1 `dot_` d1) / (d1 `dot_` d1)
  let t2 = t1 - tStep
  let (# v2, d2 #) = evaluate t2
  if Vector3D.squaredMagnitude_ v2 < 0.25 * Vector3D.squaredMagnitude_ v1
    then solve3D evaluate t2 v2 d2
    else validateSolution t1 tStep
