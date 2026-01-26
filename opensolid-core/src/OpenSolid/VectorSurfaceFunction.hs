module OpenSolid.VectorSurfaceFunction
  ( VectorSurfaceFunction
  , derivative
  )
where

import OpenSolid.CoordinateSystem (VectorSurfaceFunction)
import OpenSolid.CoordinateSystem qualified as CoordinateSystem
import OpenSolid.SurfaceParameter (SurfaceParameter)

derivative ::
  CoordinateSystem.Generic dimension units space =>
  SurfaceParameter ->
  VectorSurfaceFunction dimension units space ->
  VectorSurfaceFunction dimension units space
derivative = CoordinateSystem.vectorSurfaceFunctionDerivative
