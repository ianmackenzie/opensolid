module OpenSolid.VectorSurfaceFunction
  ( VectorSurfaceFunction
  , Exists
  , derivative
  )
where

import Data.Void (Void)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import {-# SOURCE #-} OpenSolid.SurfaceFunction2D (SurfaceFunction2D)
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.UvSpace (UvSpace)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2D (VectorSurfaceFunction2D)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2D qualified as VectorSurfaceFunction2D
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3D qualified as VectorSurfaceFunction3D

type family
  VectorSurfaceFunction dimension units space =
    vectorSurfaceFunction | vectorSurfaceFunction -> dimension units space
  where
  VectorSurfaceFunction 1 units Void = SurfaceFunction1D units
  VectorSurfaceFunction 2 units space = VectorSurfaceFunction2D units space
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
      (SurfaceFunction2D Unitless UvSpace)
      (VectorSurfaceFunction dimension units space)
      (VectorSurfaceFunction dimension units space)
  ) =>
  Exists dimension units space
  where
  derivative ::
    SurfaceParameter ->
    VectorSurfaceFunction dimension units space ->
    VectorSurfaceFunction dimension units space

instance Exists 1 units Void where
  derivative = SurfaceFunction1D.derivative

instance Exists 2 units space where
  derivative = VectorSurfaceFunction2D.derivative

instance Exists 3 units space where
  derivative = VectorSurfaceFunction3D.derivative
