module OpenSolid.SurfaceCurve3D
  ( SurfaceCurve3D (Pole, Edge)
  , IsDegenerate (IsDegenerate)
  , new
  , uvCurve
  , bounds
  , uvBounds
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Space qualified as Space
import {-# SOURCE #-} OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvCurve (UvCurve)
import OpenSolid.UvCurve qualified as UvCurve
import OpenSolid.UvPoint (UvPoint)

data SurfaceCurve3D space
  = Pole_ {function :: SurfaceFunction3D space, pole :: Surface3D.Pole space}
  | Edge_ {function :: SurfaceFunction3D space, edge :: Surface3D.Edge space}

{-# COMPLETE Pole, Edge #-}

pattern Pole :: Surface3D.Pole space -> SurfaceCurve3D space
pattern Pole pole <- Pole_{pole}

pattern Edge :: Surface3D.Edge space -> SurfaceCurve3D space
pattern Edge edge <- Edge_{edge}

data IsDegenerate = IsDegenerate UvPoint deriving (Eq, Show, Err)

instance Space.Coercion (SurfaceCurve3D space1) (SurfaceCurve3D space2) where
  coerce (Pole_ function pole) = Pole_ (Space.coerce function) (Space.coerce pole)
  coerce (Edge_ function edge) = Edge_ (Space.coerce function) (Space.coerce edge)

instance space1 ~ space2 => Bounded (SurfaceCurve3D space1) (Bounds3D space2) where
  {-# INLINE bounds #-}
  bounds = bounds

instance Bounded (SurfaceCurve3D space) UvBounds where
  {-# INLINE bounds #-}
  bounds = uvBounds

instance
  Composition
    (Tolerance Meters)
    (SurfaceCurve3D space)
    (Curve1D Unitless)
    (Result IsDegenerate (SurfaceCurve3D space))
  where
  surfaceCurve << parameterization =
    case unitless (uvCurve surfaceCurve << parameterization) of
      Ok composedUvCurve -> Ok (new surfaceCurve.function composedUvCurve)
      Err (UvCurve.IsDegenerate uvPoint) -> Err (IsDegenerate uvPoint)

new ::
  Tolerance Meters =>
  SurfaceFunction3D space ->
  UvCurve ->
  SurfaceCurve3D space
new givenFunction givenUvCurve =
  case givenFunction << givenUvCurve of
    Ok curve -> Edge_ givenFunction (Surface3D.Edge givenUvCurve curve)
    Err (Curve3D.IsDegenerate point) -> Pole_ givenFunction (Surface3D.Pole givenUvCurve point)

uvCurve :: SurfaceCurve3D space -> UvCurve
uvCurve surfaceCurve = case surfaceCurve of
  Pole (Surface3D.Pole poleUvCurve _) -> poleUvCurve
  Edge (Surface3D.Edge edgeUvCurve _) -> edgeUvCurve

bounds :: SurfaceCurve3D space -> Bounds3D space
bounds surfaceCurve = case surfaceCurve of
  Pole (Surface3D.Pole _ point) -> Bounds3D.constant point
  Edge (Surface3D.Edge _ curve) -> Curve3D.bounds curve

uvBounds :: SurfaceCurve3D space -> UvBounds
uvBounds = UvCurve.bounds . uvCurve

placeIn :: Frame3D global local -> SurfaceCurve3D local -> SurfaceCurve3D global
placeIn frame surfaceCurve = case surfaceCurve of
  Pole_ function (Surface3D.Pole poleUvCurve point) ->
    Pole_
      { function = SurfaceFunction3D.placeIn frame function
      , pole = Surface3D.Pole poleUvCurve (Point3D.placeIn frame point)
      }
  Edge_ function (Surface3D.Edge edgeUvCurve curve) ->
    Edge_
      { function = SurfaceFunction3D.placeIn frame function
      , edge = Surface3D.Edge edgeUvCurve (Curve3D.placeIn frame curve)
      }

relativeTo :: Frame3D global local -> SurfaceCurve3D global -> SurfaceCurve3D local
relativeTo frame surfaceCurve = placeIn (Frame3D.inverse frame) surfaceCurve
