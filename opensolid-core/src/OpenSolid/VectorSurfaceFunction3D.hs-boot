module OpenSolid.VectorSurfaceFunction3D
  ( VectorSurfaceFunction3D
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
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.VectorBounds3D (VectorBounds3D)

type role VectorSurfaceFunction3D nominal nominal

type VectorSurfaceFunction3D :: Type -> Type -> Type
data VectorSurfaceFunction3D units space

type Compiled units space =
  CompiledFunction
    UvPoint
    (Vector3D units space)
    UvBounds
    (VectorBounds3D units space)

instance
  HasField
    "compiled"
    (VectorSurfaceFunction3D units space)
    (Compiled units space)

instance HasUnits (VectorSurfaceFunction3D units space) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (VectorSurfaceFunction3D units1 space1)
    (VectorSurfaceFunction3D units2 space2)

instance
  Multiplication_
    (SurfaceFunction units1)
    (VectorSurfaceFunction3D units2 space)
    (VectorSurfaceFunction3D (units1 ?*? units2) space)

instance
  Multiplication_
    (VectorSurfaceFunction3D units1 space)
    (SurfaceFunction units2)
    (VectorSurfaceFunction3D (units1 ?*? units2) space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction units1)
    (VectorSurfaceFunction3D units2 space)
    (VectorSurfaceFunction3D units3 space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorSurfaceFunction3D units1 space)
    (SurfaceFunction units2)
    (VectorSurfaceFunction3D units3 space)

new ::
  Compiled units space ->
  (SurfaceParameter -> VectorSurfaceFunction3D units space) ->
  VectorSurfaceFunction3D units space
derivative ::
  SurfaceParameter ->
  VectorSurfaceFunction3D units space ->
  VectorSurfaceFunction3D units space
constant :: Vector3D units space -> VectorSurfaceFunction3D units space
