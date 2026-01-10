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
import OpenSolid.Primitives (Axis2d, Direction2d, Point2D, Transform2d, Vector2D)
import OpenSolid.Transform (Affine, Orthonormal, Rigid, Uniform)

translateByImpl ::
  (Transform2d Rigid units space -> a -> b) ->
  Vector2D units space ->
  a ->
  b
translateInImpl ::
  (Transform2d Rigid units space -> a -> b) ->
  Direction2d space ->
  Quantity units ->
  a ->
  b
translateAlongImpl ::
  (Transform2d Rigid units space -> a -> b) ->
  Axis2d units space ->
  Quantity units ->
  a ->
  b
rotateAroundImpl ::
  (Transform2d Rigid units space -> a -> b) ->
  Point2D units space ->
  Angle ->
  a ->
  b
mirrorAcrossImpl ::
  (Transform2d Orthonormal units space -> a -> b) ->
  Axis2d units space ->
  a ->
  b
scaleAboutImpl ::
  (Transform2d Uniform units space -> a -> b) ->
  Point2D units space ->
  Number ->
  a ->
  b
scaleAlongImpl ::
  (Transform2d Affine units space -> a -> b) ->
  Axis2d units space ->
  Number ->
  a ->
  b
