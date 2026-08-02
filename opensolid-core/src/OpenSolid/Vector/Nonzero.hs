module OpenSolid.Vector.Nonzero (direction) where

import OpenSolid.Direction (Direction, DirectionExists)
import OpenSolid.Direction qualified as Direction
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Prelude
import OpenSolid.Vector (Vector, VectorExists)
import OpenSolid.Vector qualified as Vector

direction ::
  (VectorExists dimension units space, DirectionExists dimension space) =>
  Nonzero (Vector dimension units space) ->
  Direction dimension space
direction (Nonzero vector) = Direction.unsafe (vector / Vector.magnitude vector)
