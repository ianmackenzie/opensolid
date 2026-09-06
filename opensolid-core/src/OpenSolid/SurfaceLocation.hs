module OpenSolid.SurfaceLocation (SurfaceLocation (Point, Pole)) where

import OpenSolid.UvEdge (UvEdge)
import OpenSolid.UvPoint (UvPoint)

data SurfaceLocation
  = Point UvPoint
  | Pole UvEdge
