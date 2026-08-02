module OpenSolid.Vector2D.Nonzero (direction) where

import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Prelude
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector2D qualified as Vector2D

direction :: Nonzero (Vector2D units) -> Direction2D
direction (Nonzero vector) = Direction2D.unsafe (vector / Vector2D.magnitude vector)
