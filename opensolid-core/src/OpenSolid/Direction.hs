module OpenSolid.Direction
  ( Direction
  , unsafe
  )
where

import OpenSolid.CoordinateSystem (Direction, DirectionCoordinateSystem, Vector)
import OpenSolid.CoordinateSystem qualified as CoordinateSystem
import OpenSolid.Prelude

unsafe ::
  DirectionCoordinateSystem dimension space =>
  Vector dimension Unitless space ->
  Direction dimension space
unsafe = CoordinateSystem.unsafeDirection
