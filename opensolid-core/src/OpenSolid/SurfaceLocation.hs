module OpenSolid.SurfaceLocation (SurfaceLocation (..)) where

import OpenSolid.CurvePoint2D (CurvePoint2D)
import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint)

data SurfaceLocation
  = Interior UvPoint
  | Boundary (CurvePoint2D Unitless)
