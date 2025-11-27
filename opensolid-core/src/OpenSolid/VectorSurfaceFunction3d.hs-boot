module OpenSolid.VectorSurfaceFunction3d
  ( VectorSurfaceFunction3d
  , Compiled
  , new
  , constant
  , derivative
  )
where

import GHC.Records (HasField)
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.VectorBounds3d (VectorBounds3d)

type role VectorSurfaceFunction3d nominal nominal

type VectorSurfaceFunction3d :: Type -> Type -> Type
data VectorSurfaceFunction3d units space

type Compiled units space =
  CompiledFunction
    UvPoint
    (Vector3d units space)
    UvBounds
    (VectorBounds3d units space)

instance
  HasField
    "compiled"
    (VectorSurfaceFunction3d units space)
    (Compiled units space)

instance HasUnits (VectorSurfaceFunction3d units space) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (VectorSurfaceFunction3d units1 space1)
    (VectorSurfaceFunction3d units2 space2)

instance
  Multiplication_
    (SurfaceFunction units1)
    (VectorSurfaceFunction3d units2 space)
    (VectorSurfaceFunction3d (units1 ?*? units2) space)

instance
  Multiplication_
    (VectorSurfaceFunction3d units1 space)
    (SurfaceFunction units2)
    (VectorSurfaceFunction3d (units1 ?*? units2) space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction units1)
    (VectorSurfaceFunction3d units2 space)
    (VectorSurfaceFunction3d units3 space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorSurfaceFunction3d units1 space)
    (SurfaceFunction units2)
    (VectorSurfaceFunction3d units3 space)

new ::
  Compiled units space ->
  (SurfaceParameter -> VectorSurfaceFunction3d units space) ->
  VectorSurfaceFunction3d units space
derivative ::
  SurfaceParameter ->
  VectorSurfaceFunction3d units space ->
  VectorSurfaceFunction3d units space
constant :: Vector3d units space -> VectorSurfaceFunction3d units space
