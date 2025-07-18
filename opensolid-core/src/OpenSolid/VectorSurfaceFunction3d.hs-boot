module OpenSolid.VectorSurfaceFunction3d
  ( VectorSurfaceFunction3d
  , Compiled
  , new
  , constant
  , derivative
  , squaredMagnitude'
  )
where

import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.VectorBounds3d (VectorBounds3d)

type role VectorSurfaceFunction3d nominal

type VectorSurfaceFunction3d :: CoordinateSystem -> Type
data VectorSurfaceFunction3d coordinateSystem

type Compiled coordinateSystem =
  CompiledFunction
    UvPoint
    (Vector3d coordinateSystem)
    UvBounds
    (VectorBounds3d coordinateSystem)

instance
  HasField
    "compiled"
    (VectorSurfaceFunction3d (space @ units))
    (Compiled (space @ units))

instance HasUnits (VectorSurfaceFunction3d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (VectorSurfaceFunction3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))

instance
  Multiplication'
    (SurfaceFunction units1)
    (VectorSurfaceFunction3d (space @ units2))
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))

instance
  Multiplication'
    (VectorSurfaceFunction3d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction units1)
    (VectorSurfaceFunction3d (space @ units2))
    (VectorSurfaceFunction3d (space @ units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorSurfaceFunction3d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction3d (space @ units3))

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorSurfaceFunction3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (SurfaceFunction (units1 :*: units2))

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (VectorSurfaceFunction3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (VectorSurfaceFunction3d (space1 @ (units1 :*: units2)))

new ::
  Compiled (space @ units) ->
  (SurfaceParameter -> VectorSurfaceFunction3d (space @ units)) ->
  VectorSurfaceFunction3d (space @ units)
derivative ::
  SurfaceParameter ->
  VectorSurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units)
constant :: Vector3d (space @ units) -> VectorSurfaceFunction3d (space @ units)
squaredMagnitude' :: VectorSurfaceFunction3d (space @ units) -> SurfaceFunction (units :*: units)
