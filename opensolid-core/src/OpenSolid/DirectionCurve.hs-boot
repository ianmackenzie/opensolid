{-# LANGUAGE AllowAmbiguousTypes #-}

module OpenSolid.DirectionCurve
  ( unsafe
  )
where

import OpenSolid.CoordinateSystem (VectorCoordinateSystem (DirectionCurve, VectorCurve))
import OpenSolid.Prelude

unsafe ::
  forall dimension units space.
  VectorCoordinateSystem dimension units space =>
  VectorCurve dimension Unitless space ->
  DirectionCurve dimension space
