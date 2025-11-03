module OpenSolid.Transform2d
  ( translateByImpl
  , translateInImpl
  , translateAlongImpl
  , rotateAroundImpl
  , mirrorAcrossImpl
  , scaleAboutImpl
  , scaleAlongImpl
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Prelude
import OpenSolid.Primitives (Axis2d, Direction2d, Point2d, Transform2d, Vector2d)
import OpenSolid.Transform (Affine, Orthonormal, Rigid, Uniform)

translateByImpl ::
  (Transform2d Rigid (space @ units) -> a -> b) ->
  Vector2d (space @ units) ->
  a ->
  b
translateInImpl ::
  (Transform2d Rigid (space @ units) -> a -> b) ->
  Direction2d space ->
  Quantity units ->
  a ->
  b
translateAlongImpl ::
  (Transform2d Rigid (space @ units) -> a -> b) ->
  Axis2d (space @ units) ->
  Quantity units ->
  a ->
  b
rotateAroundImpl ::
  (Transform2d Rigid (space @ units) -> a -> b) ->
  Point2d (space @ units) ->
  Angle ->
  a ->
  b
mirrorAcrossImpl ::
  (Transform2d Orthonormal (space @ units) -> a -> b) ->
  Axis2d (space @ units) ->
  a ->
  b
scaleAboutImpl ::
  (Transform2d Uniform (space @ units) -> a -> b) ->
  Point2d (space @ units) ->
  Number ->
  a ->
  b
scaleAlongImpl ::
  (Transform2d Affine (space @ units) -> a -> b) ->
  Axis2d (space @ units) ->
  Number ->
  a ->
  b
