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

import OpenSolid.Primitives (Basis3d, Direction3d)

upward :: Basis3d space defines -> Direction3d space
downward :: Basis3d space defines -> Direction3d space
forward :: Basis3d space defines -> Direction3d space
backward :: Basis3d space defines -> Direction3d space
rightward :: Basis3d space defines -> Direction3d space
leftward :: Basis3d space defines -> Direction3d space
