module OpenSolid.VectorSurfaceFunction
  ( VectorSurfaceFunction
  , Exists
  , partialDerivatives
  )
where

import OpenSolid.Prelude
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.SurfaceFunction2D (SurfaceFunction2D)
import OpenSolid.VectorSurfaceFunction2D (VectorSurfaceFunction2D)
import OpenSolid.VectorSurfaceFunction2D qualified as VectorSurfaceFunction2D
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)
import OpenSolid.VectorSurfaceFunction3D qualified as VectorSurfaceFunction3D

type family
  VectorSurfaceFunction dimension units space =
    vectorSurfaceFunction | vectorSurfaceFunction -> dimension units space
  where
  VectorSurfaceFunction 1 units Void = SurfaceFunction1D units
  VectorSurfaceFunction 2 units Void = VectorSurfaceFunction2D units
  VectorSurfaceFunction 3 units space = VectorSurfaceFunction3D units space

class
  ( Addition
      (VectorSurfaceFunction dimension units space)
      (VectorSurfaceFunction dimension units space)
      (VectorSurfaceFunction dimension units space)
  , Multiplication
      Number
      (VectorSurfaceFunction dimension units space)
      (VectorSurfaceFunction dimension units space)
  , Multiplication
      (VectorSurfaceFunction dimension units space)
      Number
      (VectorSurfaceFunction dimension units space)
  , Multiplication
      (SurfaceFunction1D Unitless)
      (VectorSurfaceFunction dimension units space)
      (VectorSurfaceFunction dimension units space)
  , Multiplication
      (VectorSurfaceFunction dimension units space)
      (SurfaceFunction1D Unitless)
      (VectorSurfaceFunction dimension units space)
  , Composition
      (VectorSurfaceFunction dimension units space)
      (SurfaceFunction2D Unitless)
      (VectorSurfaceFunction dimension units space)
  ) =>
  Exists dimension units space
  where
  partialDerivatives ::
    VectorSurfaceFunction dimension units space ->
    (VectorSurfaceFunction dimension units space, VectorSurfaceFunction dimension units space)

instance Exists 1 units Void where
  partialDerivatives = SurfaceFunction1D.partialDerivatives

instance Exists 2 units Void where
  partialDerivatives = VectorSurfaceFunction2D.partialDerivatives

instance Exists 3 units space where
  partialDerivatives = VectorSurfaceFunction3D.partialDerivatives
