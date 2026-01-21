module OpenSolid.DirectionCurve
  ( unsafe
  )
where

import OpenSolid.CoordinateSystem (DirectionCoordinateSystem, DirectionCurve, VectorCurve)
import OpenSolid.CoordinateSystem qualified as CoordinateSystem
import OpenSolid.Prelude

unsafe ::
  DirectionCoordinateSystem dimension space =>
  VectorCurve dimension Unitless space ->
  DirectionCurve dimension space
unsafe = CoordinateSystem.unsafeDirectionCurve
