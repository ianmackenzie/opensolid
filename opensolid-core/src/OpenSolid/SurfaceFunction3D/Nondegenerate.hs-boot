module OpenSolid.SurfaceFunction3D.Nondegenerate (segment) where

import OpenSolid.Nondegenerate (Nondegenerate)
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.SurfaceFunction3D.Segment (Segment)
import OpenSolid.UvBounds (UvBounds)

segment :: UvBounds -> Nondegenerate (SurfaceFunction3D space) -> Segment space
