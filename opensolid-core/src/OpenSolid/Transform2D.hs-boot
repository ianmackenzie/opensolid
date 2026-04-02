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
  (Transform2D Rigid units -> a -> b) ->
  Vector2D units ->
  a ->
  b
translateInImpl ::
  (Transform2D Rigid units -> a -> b) ->
  Direction2D ->
  Quantity units ->
  a ->
  b
translateAlongImpl ::
  (Transform2D Rigid units -> a -> b) ->
  Axis2D units ->
  Quantity units ->
  a ->
  b
rotateAroundImpl ::
  (Transform2D Rigid units -> a -> b) ->
  Point2D units ->
  Angle ->
  a ->
  b
mirrorAcrossImpl ::
  (Transform2D Orthonormal units -> a -> b) ->
  Axis2D units ->
  a ->
  b
scaleAboutImpl ::
  (Transform2D Uniform units -> a -> b) ->
  Point2D units ->
  Number ->
  a ->
  b
scaleAlongImpl ::
  (Transform2D Affine units -> a -> b) ->
  Axis2D units ->
  Number ->
  a ->
  b
