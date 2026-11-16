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
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D

type Function value = Number -> (# value, value #)

class Solver value where
  solveFrom :: Number -> Function value -> Fuzzy Number

solveIn :: Solver value => Interval Unitless -> Function value -> Fuzzy Number
solveIn tRange evaluate = do
  tSolution <- solveFrom (Interval.midpoint tRange) evaluate
  if unitless (tSolution ^ tRange) then Resolved tSolution else Unresolved

validateSolution :: Number -> Number -> Fuzzy Number
validateSolution t1 tStep = if unitless (tStep ~= 0.0) then Resolved t1 else Unresolved

instance Solver (Quantity units) where
  solveFrom t1 evaluate = do
    let (# v1, dv1 #) = evaluate t1
    solve1D evaluate t1 v1 dv1

solve1D :: Function (Quantity units) -> Number -> Quantity units -> Quantity units -> Fuzzy Number
solve1D evaluate t1 v1 dv1 = do
  let tStep = v1 / dv1
  let t2 = t1 - tStep
  let (# v2, dv2 #) = evaluate t2
  if Quantity.abs v2 < 0.5 * Quantity.abs v1
    then solve1D evaluate t2 v2 dv2
    else validateSolution t1 tStep

instance Solver (Vector2D units) where
  solveFrom t1 evaluate = do
    let (# v1, d1 #) = evaluate t1
    solve2D evaluate t1 v1 d1

solve2D :: Function (Vector2D units) -> Number -> Vector2D units -> Vector2D units -> Fuzzy Number
solve2D evaluate t1 v1 d1 = do
  let tStep = (v1 `dot_` d1) / (d1 `dot_` d1)
  let t2 = t1 - tStep
  let (# v2, d2 #) = evaluate t2
  if Vector2D.squaredMagnitude_ v2 < 0.25 * Vector2D.squaredMagnitude_ v1
    then solve2D evaluate t2 v2 d2
    else validateSolution t1 tStep

instance Solver (Vector3D units space) where
  solveFrom t1 evaluate = do
    let (# v1, d1 #) = evaluate t1
    solve3D evaluate t1 v1 d1

solve3D ::
  Function (Vector3D units space) ->
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
