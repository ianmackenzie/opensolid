module OpenSolid.Curve3D.SegmentOverlappingWithSurface (SegmentOverlappingWithSurface (..)) where

import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.SurfaceCurve3D (SurfaceCurve3D)

data SegmentOverlappingWithSurface space = SegmentOverlappingWithSurface
  { tBounds :: Interval Unitless
  , surfaceCurve :: SurfaceCurve3D space
  }
