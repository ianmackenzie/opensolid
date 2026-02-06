module OpenSolid.DirectionBounds
  ( DirectionBounds
  )
where

import OpenSolid.DirectionBounds2D (DirectionBounds2D)
import OpenSolid.DirectionBounds3D (DirectionBounds3D)

type family
  DirectionBounds dimension space =
    directionBounds | directionBounds -> dimension space
  where
  DirectionBounds 2 space = DirectionBounds2D space
  DirectionBounds 3 space = DirectionBounds3D space
