{-# LANGUAGE AllowAmbiguousTypes #-}

module OpenSolid.Direction
  ( Direction
  , unsafe
  )
where

import OpenSolid.CoordinateSystem (CoordinateSystem (Direction, UnitlessVector))
import OpenSolid.CoordinateSystem qualified as CoordinateSystem

unsafe ::
  forall dimension units space.
  CoordinateSystem dimension units space =>
  UnitlessVector dimension space ->
  Direction dimension space
unsafe = CoordinateSystem.unsafeDirection @dimension @units @space
