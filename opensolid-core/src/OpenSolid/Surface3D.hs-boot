module OpenSolid.Surface3D
  ( Surface3D
  , Pole
  , Edge
  , Pin
  , IsDegenerate
  , parametric
  )
where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Surface3D.Edge (Edge)
import {-# SOURCE #-} OpenSolid.Surface3D.Pin (Pin)
import {-# SOURCE #-} OpenSolid.Surface3D.Pole (Pole)
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.UvRegion (UvRegion)

type role Surface3D nominal

type Surface3D :: Type -> Type
data Surface3D space

data IsDegenerate

parametric ::
  Tolerance Meters =>
  SurfaceFunction3D space ->
  UvRegion ->
  Result IsDegenerate (Surface3D space)
