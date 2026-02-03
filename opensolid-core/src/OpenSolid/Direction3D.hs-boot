module OpenSolid.Direction3D
  ( Direction3D
  , unsafe
  , unwrap
  , upward
  , downward
  , forward
  , backward
  , rightward
  , leftward
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (Direction3D, Orientation3D, Vector3D)

unsafe :: Vector3D Unitless space -> Direction3D space
unwrap :: Direction3D space -> Vector3D Unitless space
upward :: Orientation3D space -> Direction3D space
downward :: Orientation3D space -> Direction3D space
forward :: Orientation3D space -> Direction3D space
backward :: Orientation3D space -> Direction3D space
rightward :: Orientation3D space -> Direction3D space
leftward :: Orientation3D space -> Direction3D space
