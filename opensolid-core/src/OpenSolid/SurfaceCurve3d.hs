module OpenSolid.SurfaceCurve3d
  ( SurfaceCurve3d
  , curve
  , uvCurve
  , surfaceFunction
  , on
  )
where

import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve3d (Curve3d)
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction3d (SurfaceFunction3d)

data SurfaceCurve3d space = SurfaceCurve3d
  { surfaceFunction :: SurfaceFunction3d space
  , uvCurve :: Curve2d Unitless UvSpace
  , curve :: Curve3d space
  }

on :: SurfaceFunction3d space -> Curve2d Unitless UvSpace -> SurfaceCurve3d space
on givenSurfaceFunction givenUvCurve =
  SurfaceCurve3d
    { surfaceFunction = givenSurfaceFunction
    , uvCurve = givenUvCurve
    , curve = givenSurfaceFunction `compose` givenUvCurve
    }

curve :: SurfaceCurve3d space -> Curve3d space
curve = (.curve)

uvCurve :: SurfaceCurve3d space -> Curve2d Unitless UvSpace
uvCurve = (.uvCurve)

surfaceFunction :: SurfaceCurve3d space -> SurfaceFunction3d space
surfaceFunction = (.surfaceFunction)
