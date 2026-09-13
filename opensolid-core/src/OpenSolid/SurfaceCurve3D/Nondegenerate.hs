module OpenSolid.SurfaceCurve3D.Nondegenerate
  ( curve
  , uvCurve
  )
where

import OpenSolid.Curve3D (Curve3D)
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.SurfaceCurve3D (SurfaceCurve3D)
import OpenSolid.SurfaceCurve3D qualified as SurfaceCurve3D
import OpenSolid.UvCurve (UvCurve)

curve :: Nondegenerate (SurfaceCurve3D space) -> Nondegenerate (Curve3D space)
curve (Nondegenerate surfaceCurve) = Nondegenerate (SurfaceCurve3D.curve surfaceCurve)

uvCurve :: Nondegenerate (SurfaceCurve3D space) -> Nondegenerate UvCurve
uvCurve (Nondegenerate surfaceCurve) = Nondegenerate (SurfaceCurve3D.uvCurve surfaceCurve)
