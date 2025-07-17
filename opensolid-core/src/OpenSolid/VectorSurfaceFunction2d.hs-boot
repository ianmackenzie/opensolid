-- Allow typeclass instances to be declared here
-- even though the type is actually defined in the Functions module
{-# OPTIONS_GHC -Wno-orphans #-}

module OpenSolid.VectorSurfaceFunction2d
  ( VectorSurfaceFunction2d
  , Compiled
  , new
  , constant
  , derivative
  , xComponent
  , yComponent
  , components
  , squaredMagnitude'
  )
where

import OpenSolid.Functions (VectorSurfaceFunction2d (..), VectorSurfaceFunction2dCompiled)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.Vector2d (Vector2d)

type Compiled coordinateSystem = VectorSurfaceFunction2dCompiled coordinateSystem

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorSurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction (units1 :*: units2))

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (VectorSurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction (units1 :*: units2))

new ::
  Compiled (space @ units) ->
  (SurfaceParameter -> VectorSurfaceFunction2d (space @ units)) ->
  VectorSurfaceFunction2d (space @ units)
derivative ::
  SurfaceParameter ->
  VectorSurfaceFunction2d (space @ units) ->
  VectorSurfaceFunction2d (space @ units)
constant :: Vector2d (space @ units) -> VectorSurfaceFunction2d (space @ units)
xComponent :: VectorSurfaceFunction2d (space @ units) -> SurfaceFunction units
yComponent :: VectorSurfaceFunction2d (space @ units) -> SurfaceFunction units
components ::
  VectorSurfaceFunction2d (space @ units) ->
  (SurfaceFunction units, SurfaceFunction units)
squaredMagnitude' :: VectorSurfaceFunction2d (space @ units) -> SurfaceFunction (units :*: units)
