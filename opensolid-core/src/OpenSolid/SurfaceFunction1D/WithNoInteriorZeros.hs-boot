module OpenSolid.SurfaceFunction1D.WithNoInteriorZeros (unwrap) where

import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D, WithNoInteriorZeros)

unwrap :: WithNoInteriorZeros units -> SurfaceFunction1D units
