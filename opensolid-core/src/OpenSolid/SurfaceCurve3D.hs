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
import OpenSolid.Prelude
import OpenSolid.Space qualified as Space
import {-# SOURCE #-} OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.Surface3D.Edge qualified as Surface3D.Edge
import OpenSolid.Surface3D.Pin (Pin)
import OpenSolid.Surface3D.Pole qualified as Surface3D.Pole
import OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvCurve (UvCurve)
import OpenSolid.UvCurve qualified as UvCurve

data SurfaceCurve3D space
  = Pole (Surface3D.Pole space)
  | Edge (Surface3D.Edge space)

data IsDegenerate space = IsDegenerate (Pin space) deriving (Show, Err)

instance Space.Coercion (SurfaceCurve3D space1) (SurfaceCurve3D space2) where
  coerce (Pole pole) = Pole (Space.coerce pole)
  coerce (Edge edge) = Edge (Space.coerce edge)

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
    (Result (IsDegenerate space) (SurfaceCurve3D space))
  where
  Pole pole << parameterization =
    case pole << parameterization of
      Ok composedPole -> Ok (Pole composedPole)
      Err (Surface3D.Pole.IsDegenerate pin) -> Err (IsDegenerate pin)
  Edge edge << parameterization =
    case edge << parameterization of
      Ok composedEdge -> Ok (Edge composedEdge)
      Err (Surface3D.Edge.IsDegenerate pin) -> Err (IsDegenerate pin)

new ::
  Tolerance Meters =>
  SurfaceFunction3D space ->
  UvCurve ->
  SurfaceCurve3D space
new givenFunction givenUvCurve =
  case givenFunction << givenUvCurve of
    Ok curve -> Edge (Surface3D.Edge.with {uvCurve = givenUvCurve, curve})
    Err (Curve3D.IsDegenerate point) -> Pole (Surface3D.Pole.with {uvCurve = givenUvCurve, point})

uvCurve :: SurfaceCurve3D space -> UvCurve
uvCurve surfaceCurve = case surfaceCurve of
  Pole pole -> Surface3D.Pole.uvCurve pole
  Edge edge -> Surface3D.Edge.uvCurve edge

bounds :: SurfaceCurve3D space -> Bounds3D space
bounds surfaceCurve = case surfaceCurve of
  Pole pole -> Bounds3D.constant (Surface3D.Pole.point pole)
  Edge edge -> Curve3D.bounds (Surface3D.Edge.curve edge)

uvBounds :: SurfaceCurve3D space -> UvBounds
uvBounds = UvCurve.bounds . uvCurve

placeIn :: Frame3D global local -> SurfaceCurve3D local -> SurfaceCurve3D global
placeIn frame surfaceCurve = case surfaceCurve of
  Pole pole -> Pole (Surface3D.Pole.placeIn frame pole)
  Edge edge -> Edge (Surface3D.Edge.placeIn frame edge)

relativeTo :: Frame3D global local -> SurfaceCurve3D global -> SurfaceCurve3D local
relativeTo frame surfaceCurve = placeIn (Frame3D.inverse frame) surfaceCurve
