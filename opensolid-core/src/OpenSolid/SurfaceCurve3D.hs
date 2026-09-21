module OpenSolid.SurfaceCurve3D
  ( SurfaceCurve3D
  , new
  , curve
  , uvCurve
  , bounds
  , uvBounds
  , nondegenerate
  )
where

import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Curve3D (Curve3D)
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.IsDegenerate (IsDegenerate (IsDegenerate))
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Prelude
import OpenSolid.Space qualified as Space
import OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.SurfacePoint3D (SurfacePoint3D)
import OpenSolid.SurfacePoint3D qualified as SurfacePoint3D
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvCurve (UvCurve)
import OpenSolid.UvCurve qualified as UvCurve

data SurfaceCurve3D space = SurfaceCurve3D
  { uvCurve :: UvCurve
  , curve :: Curve3D space
  }

instance Space.Coercion (SurfaceCurve3D space1) (SurfaceCurve3D space2) where
  coerce surfaceCurve =
    SurfaceCurve3D
      { uvCurve = surfaceCurve.uvCurve
      , curve = Space.coerce surfaceCurve.curve
      }

instance space1 ~ space2 => Bounded (SurfaceCurve3D space1) (Bounds3D space2) where
  {-# INLINE bounds #-}
  bounds = bounds

instance space1 ~ space2 => Bounded (Nondegenerate (SurfaceCurve3D space1)) (Bounds3D space2) where
  {-# INLINE bounds #-}
  bounds (Nondegenerate surfaceCurve) = bounds surfaceCurve

instance Bounded (SurfaceCurve3D space) UvBounds where
  {-# INLINE bounds #-}
  bounds = uvBounds

instance Bounded (Nondegenerate (SurfaceCurve3D space)) UvBounds where
  {-# INLINE bounds #-}
  bounds (Nondegenerate surfaceCurve) = uvBounds surfaceCurve

instance Composition () (SurfaceCurve3D space) (Curve1D Unitless) (SurfaceCurve3D space) where
  surfaceCurve << parameterization =
    SurfaceCurve3D
      { uvCurve = uvCurve surfaceCurve << parameterization
      , curve = curve surfaceCurve << parameterization
      }

new :: SurfaceFunction3D space -> UvCurve -> SurfaceCurve3D space
new givenSurfaceFunction givenUvCurve =
  SurfaceCurve3D
    { uvCurve = givenUvCurve
    , curve = givenSurfaceFunction << givenUvCurve
    }

curve :: SurfaceCurve3D space -> Curve3D space
curve = (.curve)

uvCurve :: SurfaceCurve3D space -> UvCurve
uvCurve = (.uvCurve)

bounds :: SurfaceCurve3D space -> Bounds3D space
bounds = Curve3D.bounds . curve

uvBounds :: SurfaceCurve3D space -> UvBounds
uvBounds = Curve2D.bounds . uvCurve

nondegenerate ::
  Tolerance Meters =>
  SurfaceCurve3D space ->
  Result (IsDegenerate (SurfacePoint3D space)) (Nondegenerate (SurfaceCurve3D space))
nondegenerate surfaceCurve =
  case Curve.nondegenerate (curve surfaceCurve) of
    -- Assume that if 3D curve is nondegenerate, UV curve must be too
    Ok Nondegenerate{} -> Ok (Nondegenerate surfaceCurve)
    -- 3D curve is degenerate: check UV curve
    Err (IsDegenerate point) ->
      Err . IsDegenerate $
        case UvCurve.nondegenerate (uvCurve surfaceCurve) of
          Ok nondegenerateUvCurve ->
            SurfacePoint3D.Pole nondegenerateUvCurve point
          Err (IsDegenerate uvPoint) ->
            SurfacePoint3D.Point uvPoint point
