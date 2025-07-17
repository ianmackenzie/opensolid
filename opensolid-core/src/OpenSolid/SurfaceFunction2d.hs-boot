-- Allow typeclass instances to be declared here
-- even though the type is actually defined in the Functions module
{-# OPTIONS_GHC -Wno-orphans #-}

module OpenSolid.SurfaceFunction2d
  ( SurfaceFunction2d
  , Compiled
  , derivative
  , new
  , xCoordinate
  , yCoordinate
  )
where

import OpenSolid.Functions (SurfaceFunction2d (..), SurfaceFunction2dCompiled)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceParameter (SurfaceParameter)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)

type Compiled coordinateSystem = SurfaceFunction2dCompiled coordinateSystem

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (SurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction2d (space1 @ units1))

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (SurfaceFunction2d (space1 @ units1))
    (SurfaceFunction2d (space2 @ units2))
    (VectorSurfaceFunction2d (space1 @ units1))

new ::
  Compiled (space @ units) ->
  (SurfaceParameter -> VectorSurfaceFunction2d (space @ units)) ->
  SurfaceFunction2d (space @ units)
derivative ::
  SurfaceParameter ->
  SurfaceFunction2d (space @ units) ->
  VectorSurfaceFunction2d (space @ units)
xCoordinate :: SurfaceFunction2d (space @ units) -> SurfaceFunction units
yCoordinate :: SurfaceFunction2d (space @ units) -> SurfaceFunction units
