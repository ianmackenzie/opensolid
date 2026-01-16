{-# LANGUAGE AllowAmbiguousTypes #-}

module OpenSolid.DirectionCurve
  ( unsafe
  )
where

import OpenSolid.CoordinateSystem (CoordinateSystem (DirectionCurve, UnitlessVectorCurve))
import OpenSolid.CoordinateSystem qualified as CoordinateSystem

unsafe ::
  forall dimension units space.
  CoordinateSystem dimension units space =>
  UnitlessVectorCurve dimension space ->
  DirectionCurve dimension space
unsafe = CoordinateSystem.unsafeDirectionCurve @dimension @units @space
