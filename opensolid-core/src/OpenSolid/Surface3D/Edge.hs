module OpenSolid.Surface3D.Edge
  ( Edge
  , IsDegenerate (IsDegenerate)
  , with
  , curve
  , uvCurve
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve3D (Curve3D)
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Space qualified as Space
import OpenSolid.Surface3D.Pin (Pin)
import OpenSolid.Surface3D.Pin qualified as Surface3D.Pin
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvCurve (UvCurve)
import OpenSolid.UvCurve qualified as UvCurve

data Edge space = Edge
  { uvCurve :: UvCurve
  , curve :: Curve3D space
  }
  deriving (Show)

instance Space.Coercion (Edge space1) (Edge space2) where
  coerce edge =
    Edge
      { uvCurve = uvCurve edge
      , curve = Space.coerce (curve edge)
      }

instance Bounded (Edge space) UvBounds where
  {-# INLINE bounds #-}
  bounds edge = UvCurve.bounds (uvCurve edge)

instance space1 ~ space2 => Bounded (Edge space1) (Bounds3D space2) where
  {-# INLINE bounds #-}
  bounds pole = Curve3D.bounds (curve pole)

instance At (Edge space) Number (Pin space) where
  {-# INLINE (@) #-}
  edge @ tValue = Surface3D.Pin.with {uvPoint = uvCurve edge @ tValue, point = curve edge @ tValue}

data IsDegenerate space = IsDegenerate (Pin space) deriving (Show, Err)

instance
  Composition
    ()
    (Edge space)
    (Curve1D Unitless)
    (Result (IsDegenerate space) (Edge space))
  where
  edge << parameterization =
    case unitless (Curve1D.asConstant parameterization) of
      Just tValue -> do
        let pin = Surface3D.Pin.with {uvPoint = uvCurve edge @ tValue, point = curve edge @ tValue}
        Err $ IsDegenerate pin
      Nothing -> do
        let panic = error "Composition with non-constant parameterization should not fail"
        let composedUvCurve =
              Tolerance.using Quantity.zero (uvCurve edge << parameterization) !! panic
        let composedCurve =
              Tolerance.using Quantity.zero (curve edge << parameterization) !! panic
        Ok $ Edge {uvCurve = composedUvCurve, curve = composedCurve}

with :: ("uvCurve" ::: UvCurve, "curve" ::: Curve3D space) -> Edge space
with params = Edge {uvCurve = params.uvCurve, curve = params.curve}

uvCurve :: Edge space -> UvCurve
uvCurve edge = edge.uvCurve

curve :: Edge space -> Curve3D space
curve edge = edge.curve

placeIn :: Frame3D global local -> Edge local -> Edge global
placeIn frame edge =
  Edge
    { uvCurve = uvCurve edge
    , curve = Curve3D.placeIn frame (curve edge)
    }

relativeTo :: Frame3D global local -> Edge global -> Edge local
relativeTo frame edge = placeIn (Frame3D.inverse frame) edge
