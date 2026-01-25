module OpenSolid.VectorSurfaceFunction2D
  ( VectorSurfaceFunction2D
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
import {-# SOURCE #-} OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.Prelude
import OpenSolid.Primitives (Vector2D, VectorBounds2D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.UvBounds (UvBounds)
import {-# SOURCE #-} OpenSolid.UvPoint (UvPoint)

type role VectorSurfaceFunction2D nominal nominal

type VectorSurfaceFunction2D :: Type -> Type -> Type
data VectorSurfaceFunction2D units space

type Compiled units space =
  CompiledFunction
    UvPoint
    (Vector2D units space)
    UvBounds
    (VectorBounds2D units space)

instance
  HasField
    "compiled"
    (VectorSurfaceFunction2D units space)
    (Compiled units space)

instance HasUnits (VectorSurfaceFunction2D units space) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (VectorSurfaceFunction2D units1 space1)
    (VectorSurfaceFunction2D units2 space2)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction1D units1)
    (VectorSurfaceFunction2D units2 space)
    (VectorSurfaceFunction2D units3 space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorSurfaceFunction2D units1 space)
    (SurfaceFunction1D units2)
    (VectorSurfaceFunction2D units3 space)

instance
  Multiplication_
    (SurfaceFunction1D units1)
    (VectorSurfaceFunction2D units2 space)
    (VectorSurfaceFunction2D (units1 ?*? units2) space)

instance
  Multiplication_
    (VectorSurfaceFunction2D units1 space)
    (SurfaceFunction1D units2)
    (VectorSurfaceFunction2D (units1 ?*? units2) space)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorSurfaceFunction2D units1 space1)
    (VectorSurfaceFunction2D units2 space2)
    (SurfaceFunction1D (units1 ?*? units2))

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorSurfaceFunction2D units1 space1)
    (VectorSurfaceFunction2D units2 space2)
    (SurfaceFunction1D (units1 ?*? units2))

new ::
  Compiled units space ->
  (SurfaceParameter -> VectorSurfaceFunction2D units space) ->
  VectorSurfaceFunction2D units space
derivative ::
  SurfaceParameter ->
  VectorSurfaceFunction2D units space ->
  VectorSurfaceFunction2D units space
constant :: Vector2D units space -> VectorSurfaceFunction2D units space
xComponent :: VectorSurfaceFunction2D units space -> SurfaceFunction1D units
yComponent :: VectorSurfaceFunction2D units space -> SurfaceFunction1D units
components ::
  VectorSurfaceFunction2D units space ->
  (SurfaceFunction1D units, SurfaceFunction1D units)
squaredMagnitude_ :: VectorSurfaceFunction2D units space -> SurfaceFunction1D (units ?*? units)
