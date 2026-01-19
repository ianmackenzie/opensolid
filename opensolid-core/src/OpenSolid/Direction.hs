{-# LANGUAGE AllowAmbiguousTypes #-}

module OpenSolid.Direction
  ( Direction
  , unsafe
  )
where

import OpenSolid.CoordinateSystem (VectorCoordinateSystem (Direction, Vector))
import OpenSolid.CoordinateSystem qualified as CoordinateSystem
import OpenSolid.Prelude

unsafe ::
  forall dimension units space.
  VectorCoordinateSystem dimension units space =>
  Vector dimension Unitless space ->
  Direction dimension space
unsafe = CoordinateSystem.unsafeDirection @dimension @units @space
