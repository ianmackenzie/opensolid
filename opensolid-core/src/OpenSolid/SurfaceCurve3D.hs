module OpenSolid.SurfaceCurve3D
  ( SurfaceCurve3D
  , new
  , curve
  , uvCurve
  , surfaceFunction
  , bounds
  , uvBounds
  )
where

import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Curve3D (Curve3D)
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.UvBounds (UvBounds)

data SurfaceCurve3D space = SurfaceCurve3D
  { surfaceFunction :: SurfaceFunction3D space
  , uvCurve :: Curve2D Unitless
  , curve :: Curve3D space
  }

new :: SurfaceFunction3D space -> Curve2D Unitless -> SurfaceCurve3D space
new givenSurfaceFunction givenUvCurve =
  SurfaceCurve3D
    { surfaceFunction = givenSurfaceFunction
    , uvCurve = givenUvCurve
    , curve = givenSurfaceFunction . givenUvCurve
    }

curve :: SurfaceCurve3D space -> Curve3D space
curve = (.curve)

uvCurve :: SurfaceCurve3D space -> Curve2D Unitless
uvCurve = (.uvCurve)

surfaceFunction :: SurfaceCurve3D space -> SurfaceFunction3D space
surfaceFunction = (.surfaceFunction)

bounds :: SurfaceCurve3D space -> Bounds3D space
bounds = Curve3D.bounds . curve

uvBounds :: SurfaceCurve3D space -> UvBounds
uvBounds = Curve2D.bounds . uvCurve
