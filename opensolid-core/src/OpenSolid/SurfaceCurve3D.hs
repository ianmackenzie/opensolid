module OpenSolid.SurfaceCurve3D
  ( SurfaceCurve3D
  , curve
  , uvCurve
  , surfaceFunction
  , on
  )
where

import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve3D (Curve3D)
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.UvSpace (UvSpace)

data SurfaceCurve3D space = SurfaceCurve3D
  { surfaceFunction :: SurfaceFunction3D space
  , uvCurve :: Curve2D Unitless UvSpace
  , curve :: Curve3D space
  }

on :: SurfaceFunction3D space -> Curve2D Unitless UvSpace -> SurfaceCurve3D space
on givenSurfaceFunction givenUvCurve =
  SurfaceCurve3D
    { surfaceFunction = givenSurfaceFunction
    , uvCurve = givenUvCurve
    , curve = givenSurfaceFunction . givenUvCurve
    }

curve :: SurfaceCurve3D space -> Curve3D space
curve = (.curve)

uvCurve :: SurfaceCurve3D space -> Curve2D Unitless UvSpace
uvCurve = (.uvCurve)

surfaceFunction :: SurfaceCurve3D space -> SurfaceFunction3D space
surfaceFunction = (.surfaceFunction)
