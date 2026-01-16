{-# LANGUAGE AllowAmbiguousTypes #-}

module OpenSolid.DirectionCurve
  ( unsafe
  )
where

import OpenSolid.CoordinateSystem (CoordinateSystem (DirectionCurve, UnitlessVectorCurve))

unsafe ::
  forall dimension units space.
  CoordinateSystem dimension units space =>
  UnitlessVectorCurve dimension space ->
  DirectionCurve dimension space
