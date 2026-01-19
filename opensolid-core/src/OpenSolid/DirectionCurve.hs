{-# LANGUAGE AllowAmbiguousTypes #-}

module OpenSolid.DirectionCurve
  ( unsafe
  )
where

import OpenSolid.CoordinateSystem (VectorCoordinateSystem (DirectionCurve, VectorCurve))
import OpenSolid.CoordinateSystem qualified as CoordinateSystem
import OpenSolid.Prelude

unsafe ::
  forall dimension units space.
  VectorCoordinateSystem dimension units space =>
  VectorCurve dimension Unitless space ->
  DirectionCurve dimension space
unsafe = CoordinateSystem.unsafeDirectionCurve @dimension @units @space
