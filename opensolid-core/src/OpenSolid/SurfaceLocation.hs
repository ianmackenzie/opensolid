module OpenSolid.SurfaceLocation (SurfaceLocation (Point, Pole)) where

import OpenSolid.UvCurve (UvCurve)
import OpenSolid.UvPoint (UvPoint)

data SurfaceLocation
  = Point UvPoint
  | Pole UvCurve
