{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.NewtonRaphson.Surface
  ( Function
  , Solver
  , solveFrom
  , solveIn
  )
where

import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Prelude
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D

type Function value = UvPoint -> (# value, value, value #)

class Solver value where
  solveFrom :: UvPoint -> Function value -> Fuzzy UvPoint

solveIn :: Solver value => UvBounds -> Function value -> Fuzzy UvPoint
solveIn uvRange evaluate = do
  uvSolution <- solveFrom (Bounds2D.centerPoint uvRange) evaluate
  if unitless (uvSolution ^ uvRange) then Resolved uvSolution else Unresolved

instance Solver (Vector2D units) where
  solveFrom uvPoint1 evaluate = do
    let (# v1, du1, dv1 #) = evaluate uvPoint1
    solve2D evaluate uvPoint1 v1 du1 dv1

solve2D ::
  Function (Vector2D units) ->
  UvPoint ->
  Vector2D units ->
  Vector2D units ->
  Vector2D units ->
  Fuzzy UvPoint
solve2D evaluate uvPoint1 v1 du1 dv1 = do
  let Vector2D x1 y1 = v1
  let Vector2D dxdu1 dydu1 = du1
  let Vector2D dxdv1 dydv1 = dv1
  let determinant = dxdu1 ?*? dydv1 - dxdv1 ?*? dydu1
  let uStep = (dxdv1 ?*? y1 - dydv1 ?*? x1) / determinant
  let vStep = (dydu1 ?*? x1 - dxdu1 ?*? y1) / determinant
  let uvStep = Vector2D uStep vStep
  let uvPoint2 = uvPoint1 + uvStep
  let (# v2, du2, dv2 #) = evaluate uvPoint2
  if Vector2D.squaredMagnitude_ v2 < 0.25 * Vector2D.squaredMagnitude_ v1
    then solve2D evaluate uvPoint2 v2 du2 dv2
    else validateSolution uvPoint1 uvStep

validateSolution :: UvPoint -> Vector2D Unitless -> Fuzzy UvPoint
validateSolution uvPoint uvStep =
  if unitless (uvStep ~= Vector2D.zero) then Resolved uvPoint else Unresolved

instance Solver (Vector3D units space) where
  solveFrom uvPoint1 evaluate = do
    let (# v1, du1, dv1 #) = evaluate uvPoint1
    solve3D evaluate uvPoint1 v1 du1 dv1

solve3D ::
  Function (Vector3D units space) ->
  UvPoint ->
  Vector3D units space ->
  Vector3D units space ->
  Vector3D units space ->
  Fuzzy UvPoint
solve3D evaluate uvPoint1 v1 du1 dv1 = do
  let uu = du1 `dot_` du1
  let uv = du1 `dot_` dv1
  let vv = dv1 `dot_` dv1
  let determinant = uu ?*? vv - uv ?*? uv
  let x = v1 `dot_` du1
  let y = v1 `dot_` dv1
  let uStep = (uv ?*? y - vv ?*? x) / determinant
  let vStep = (uv ?*? x - uu ?*? y) / determinant
  let uvStep = Vector2D uStep vStep
  let uvPoint2 = uvPoint1 + uvStep
  let (# v2, du2, dv2 #) = evaluate uvPoint2
  if Vector3D.squaredMagnitude_ v2 < 0.25 * Vector3D.squaredMagnitude_ v1
    then solve3D evaluate uvPoint2 v2 du2 dv2
    else validateSolution uvPoint1 uvStep
