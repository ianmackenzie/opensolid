module OpenSolid.DirectionCurve
  ( unsafe
  )
where

import OpenSolid.CoordinateSystem (DirectionCurve, VectorCurve)
import OpenSolid.CoordinateSystem qualified as CoordinateSystem
import OpenSolid.Prelude

unsafe ::
  CoordinateSystem.Unitless dimension space =>
  VectorCurve dimension Unitless space ->
  DirectionCurve dimension space
