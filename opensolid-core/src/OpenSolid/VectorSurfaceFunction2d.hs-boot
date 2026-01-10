module OpenSolid.VectorSurfaceFunction2d
  ( VectorSurfaceFunction2d
  , Compiled
  , new
  , constant
  , derivative
  , xComponent
  , yComponent
  , components
  , squaredMagnitude_
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
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.VectorBounds2d (VectorBounds2d)

type role VectorSurfaceFunction2d nominal nominal

type VectorSurfaceFunction2d :: Type -> Type -> Type
data VectorSurfaceFunction2d units space

type Compiled units space =
  CompiledFunction
    UvPoint
    (Vector2D units space)
    UvBounds
    (VectorBounds2d units space)

instance
  HasField
    "compiled"
    (VectorSurfaceFunction2d units space)
    (Compiled units space)

instance HasUnits (VectorSurfaceFunction2d units space) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (VectorSurfaceFunction2d units1 space1)
    (VectorSurfaceFunction2d units2 space2)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction units1)
    (VectorSurfaceFunction2d units2 space)
    (VectorSurfaceFunction2d units3 space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorSurfaceFunction2d units1 space)
    (SurfaceFunction units2)
    (VectorSurfaceFunction2d units3 space)

instance
  Multiplication_
    (SurfaceFunction units1)
    (VectorSurfaceFunction2d units2 space)
    (VectorSurfaceFunction2d (units1 ?*? units2) space)

instance
  Multiplication_
    (VectorSurfaceFunction2d units1 space)
    (SurfaceFunction units2)
    (VectorSurfaceFunction2d (units1 ?*? units2) space)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorSurfaceFunction2d units1 space1)
    (VectorSurfaceFunction2d units2 space2)
    (SurfaceFunction (units1 ?*? units2))

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorSurfaceFunction2d units1 space1)
    (VectorSurfaceFunction2d units2 space2)
    (SurfaceFunction (units1 ?*? units2))

new ::
  Compiled units space ->
  (SurfaceParameter -> VectorSurfaceFunction2d units space) ->
  VectorSurfaceFunction2d units space
derivative ::
  SurfaceParameter ->
  VectorSurfaceFunction2d units space ->
  VectorSurfaceFunction2d units space
constant :: Vector2D units space -> VectorSurfaceFunction2d units space
xComponent :: VectorSurfaceFunction2d units space -> SurfaceFunction units
yComponent :: VectorSurfaceFunction2d units space -> SurfaceFunction units
components ::
  VectorSurfaceFunction2d units space ->
  (SurfaceFunction units, SurfaceFunction units)
squaredMagnitude_ :: VectorSurfaceFunction2d units space -> SurfaceFunction (units ?*? units)
