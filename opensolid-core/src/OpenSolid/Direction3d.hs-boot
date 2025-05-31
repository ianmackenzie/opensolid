module OpenSolid.Direction3d
  ( Direction3d
  , upward
  , downward
  , forward
  , backward
  , rightward
  , leftward
  )
where

import OpenSolid.Primitives (Orientation3d, Direction3d)

upward :: Orientation3d space defines -> Direction3d space
downward :: Orientation3d space defines -> Direction3d space
forward :: Orientation3d space defines -> Direction3d space
backward :: Orientation3d space defines -> Direction3d space
rightward :: Orientation3d space defines -> Direction3d space
leftward :: Orientation3d space defines -> Direction3d space
