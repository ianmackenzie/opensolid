module OpenSolid.VectorSurfaceFunction2d
  ( VectorSurfaceFunction2d
  , Compiled
  , new
  , constant
  , derivative
  , xComponent
  , yComponent
  , components
  , squaredMagnitude#
  )
where

import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.VectorBounds2d (VectorBounds2d)

type role VectorSurfaceFunction2d nominal

type VectorSurfaceFunction2d :: CoordinateSystem -> Type
data VectorSurfaceFunction2d coordinateSystem

type Compiled coordinateSystem =
  CompiledFunction
    UvPoint
    (Vector2d coordinateSystem)
    UvBounds
    (VectorBounds2d coordinateSystem)

instance
  HasField
    "compiled"
    (VectorSurfaceFunction2d (space @ units))
    (Compiled (space @ units))

instance HasUnits (VectorSurfaceFunction2d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (VectorSurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction units1)
    (VectorSurfaceFunction2d (space @ units2))
    (VectorSurfaceFunction2d (space @ units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorSurfaceFunction2d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction2d (space @ units3))

instance
  Multiplication#
    (SurfaceFunction units1)
    (VectorSurfaceFunction2d (space @ units2))
    (VectorSurfaceFunction2d (space @ (units1 *# units2)))

instance
  Multiplication#
    (VectorSurfaceFunction2d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction2d (space @ (units1 *# units2)))

instance
  space1 ~ space2 =>
  DotMultiplication#
    (VectorSurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction (units1 *# units2))

instance
  space1 ~ space2 =>
  CrossMultiplication#
    (VectorSurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction (units1 *# units2))

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
squaredMagnitude# :: VectorSurfaceFunction2d (space @ units) -> SurfaceFunction (units *# units)
