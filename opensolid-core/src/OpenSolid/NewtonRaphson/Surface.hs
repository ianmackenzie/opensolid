{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.NewtonRaphson.Surface
  ( Function
  , Solver
  , solveFrom
  , solveIn
  )
where

import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector (Vector)
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D

type Function dimension units space =
  UvPoint ->
  (# Vector dimension units space, Vector dimension units space, Vector dimension units space #)

class Solver dimension units space where
  solveFrom :: UvPoint -> Function dimension units space -> Fuzzy UvPoint

solveIn ::
  Solver dimension units space =>
  UvBounds ->
  Function dimension units space ->
  Fuzzy UvPoint
solveIn uvRange evaluate = do
  uvSolution <- solveFrom (Bounds2D.centerPoint uvRange) evaluate
  if Bounds2D.inclusion uvSolution uvRange >= 0.0
    then Resolved uvSolution
    else Unresolved

instance Solver 2 units Void where
  solveFrom uvPoint1 evaluate = do
    let (# v1, du1, dv1 #) = evaluate uvPoint1
    solve2D evaluate uvPoint1 v1 du1 dv1

solve2D ::
  Function 2 units Void ->
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
validateSolution uvPoint uvStep = do
  let stepSize = Vector2D.magnitude uvStep
  if stepSize <= Tolerance.unitless || Number.isNaN stepSize
    then Resolved uvPoint
    else Unresolved

instance Solver 3 units space where
  solveFrom uvPoint1 evaluate = do
    let (# v1, du1, dv1 #) = evaluate uvPoint1
    solve3D evaluate uvPoint1 v1 du1 dv1

solve3D ::
  Function 3 units space ->
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
