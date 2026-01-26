module OpenSolid.Direction
  ( Direction
  , unsafe
  )
where

import OpenSolid.CoordinateSystem (Direction, Vector)
import OpenSolid.CoordinateSystem qualified as CoordinateSystem
import OpenSolid.Prelude

unsafe ::
  CoordinateSystem.Directional dimension space =>
  Vector dimension Unitless space ->
  Direction dimension space
unsafe = CoordinateSystem.unsafeDirection
