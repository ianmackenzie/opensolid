module OpenSolid.SurfaceFunction1D.WithNoInteriorZeros (unwrap) where

import OpenSolid.SurfaceFunction1D (SurfaceFunction1D, WithNoInteriorZeros (WithNoInteriorZeros))

{-# INLINE unwrap #-}
unwrap :: WithNoInteriorZeros units -> SurfaceFunction1D units
unwrap (WithNoInteriorZeros function) = function
