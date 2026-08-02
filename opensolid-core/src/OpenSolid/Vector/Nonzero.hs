module OpenSolid.Vector.Nonzero (direction) where

import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Prelude
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector

direction ::
  (Vector.Exists dimension units space, Direction.Exists dimension space) =>
  Nonzero (Vector dimension units space) ->
  Direction dimension space
direction (Nonzero vector) = Direction.unsafe (vector / Vector.magnitude vector)
