module OpenSolid.Direction3D
  ( Direction3D
  , upward
  , downward
  , forward
  , backward
  , rightward
  , leftward
  )
where

import OpenSolid.Primitives (Direction3D, Orientation3D)

upward :: Orientation3D space -> Direction3D space
downward :: Orientation3D space -> Direction3D space
forward :: Orientation3D space -> Direction3D space
backward :: Orientation3D space -> Direction3D space
rightward :: Orientation3D space -> Direction3D space
leftward :: Orientation3D space -> Direction3D space
