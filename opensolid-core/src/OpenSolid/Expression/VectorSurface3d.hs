module OpenSolid.Expression.VectorSurface3d
  ( constant
  , xyz
  , xComponent
  , yComponent
  , zComponent
  , squaredMagnitude
  , squaredMagnitude'
  , magnitude
  , interpolateFrom
  , placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  , transformBy
  )
where

import OpenSolid.Basis3d (Basis3d)
import OpenSolid.Basis3d qualified as Basis3d
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (UvPoint)
import OpenSolid.Transform3d (Transform3d (Transform3d))
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d

constant :: Vector3d (space @ units) -> Expression UvPoint (Vector3d (space @ units))
constant = Expression.constant

xyz ::
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Vector3d (space @ units))
xyz = Expression.xyz

xComponent :: Expression UvPoint (Vector3d (space @ units)) -> Expression UvPoint (Qty units)
xComponent = Expression.xComponent

yComponent :: Expression UvPoint (Vector3d (space @ units)) -> Expression UvPoint (Qty units)
yComponent = Expression.yComponent

zComponent :: Expression UvPoint (Vector3d (space @ units)) -> Expression UvPoint (Qty units)
zComponent = Expression.zComponent

squaredMagnitude' ::
  Expression UvPoint (Vector3d (space @ units)) ->
  Expression UvPoint (Qty (units :*: units))
squaredMagnitude' vector =
  Expression.squared' (xComponent vector)
    + Expression.squared' (yComponent vector)
    + Expression.squared' (zComponent vector)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Expression UvPoint (Vector3d (space @ units1)) ->
  Expression UvPoint (Qty units2)
squaredMagnitude = Units.specialize . squaredMagnitude'

magnitude :: Expression UvPoint (Vector3d (space @ units)) -> Expression UvPoint (Qty units)
magnitude = Expression.sqrt' . squaredMagnitude'

interpolateFrom ::
  Expression UvPoint (Vector3d (space @ units)) ->
  Expression UvPoint (Vector3d (space @ units)) ->
  Expression UvPoint Float ->
  Expression UvPoint (Vector3d (space @ units))
interpolateFrom start end t = start + t * (end - start)

placeIn ::
  Frame3d (global @ originPointUnits) (Defines local) ->
  Expression UvPoint (Vector3d (local @ units)) ->
  Expression
    UvPoint
    (Vector3d (global @ units))
placeIn frame expression = placeInBasis (Frame3d.basis frame) expression

relativeTo ::
  Frame3d (global @ originPointUnits) (Defines local) ->
  Expression UvPoint (Vector3d (global @ units)) ->
  Expression
    UvPoint
    (Vector3d (local @ units))
relativeTo frame expression = relativeToBasis (Frame3d.basis frame) expression

placeInBasis ::
  Basis3d global (Defines local) ->
  Expression UvPoint (Vector3d (local @ units)) ->
  Expression UvPoint (Vector3d (global @ units))
placeInBasis basis expression = do
  let i = Vector3d.unit (Basis3d.xDirection basis)
  let j = Vector3d.unit (Basis3d.yDirection basis)
  let k = Vector3d.unit (Basis3d.zDirection basis)
  xComponent expression * constant i
    + yComponent expression * constant j
    + zComponent expression * constant k

relativeToBasis ::
  Basis3d global (Defines local) ->
  Expression UvPoint (Vector3d (global @ units)) ->
  Expression UvPoint (Vector3d (local @ units))
relativeToBasis basis expression = do
  let i = Vector3d.unit (Basis3d.xDirection basis)
  let j = Vector3d.unit (Basis3d.yDirection basis)
  let k = Vector3d.unit (Basis3d.zDirection basis)
  xyz (expression <> constant i) (expression <> constant j) (expression <> constant k)

transformBy ::
  Transform3d a (space @ translationUnits) ->
  Expression UvPoint (Vector3d (space @ units)) ->
  Expression UvPoint (Vector3d (space @ units))
transformBy (Transform3d _ i j k) expression =
  xComponent expression * constant i
    + yComponent expression * constant j
    + yComponent expression * constant k
