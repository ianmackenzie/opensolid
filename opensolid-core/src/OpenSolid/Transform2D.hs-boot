module OpenSolid.Transform2D
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
import OpenSolid.Primitives (Axis2D, Direction2D, Point2D, Transform2D, Vector2D)
import OpenSolid.Transform (Affine, Orthonormal, Rigid, Uniform)

translateByImpl ::
  (Transform2D Rigid units space -> a -> b) ->
  Vector2D units space ->
  a ->
  b
translateInImpl ::
  (Transform2D Rigid units space -> a -> b) ->
  Direction2D space ->
  Quantity units ->
  a ->
  b
translateAlongImpl ::
  (Transform2D Rigid units space -> a -> b) ->
  Axis2D units space ->
  Quantity units ->
  a ->
  b
rotateAroundImpl ::
  (Transform2D Rigid units space -> a -> b) ->
  Point2D units space ->
  Angle ->
  a ->
  b
mirrorAcrossImpl ::
  (Transform2D Orthonormal units space -> a -> b) ->
  Axis2D units space ->
  a ->
  b
scaleAboutImpl ::
  (Transform2D Uniform units space -> a -> b) ->
  Point2D units space ->
  Number ->
  a ->
  b
scaleAlongImpl ::
  (Transform2D Affine units space -> a -> b) ->
  Axis2D units space ->
  Number ->
  a ->
  b
