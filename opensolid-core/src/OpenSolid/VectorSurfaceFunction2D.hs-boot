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

type role VectorSurfaceFunction2D nominal

type VectorSurfaceFunction2D :: Type -> Type
data VectorSurfaceFunction2D units

type Compiled units =
  CompiledFunction UvPoint (Vector2D units) UvBounds (VectorBounds2D units)

instance
  HasField
    "compiled"
    (VectorSurfaceFunction2D units)
    (Compiled units)

instance HasUnits (VectorSurfaceFunction2D units) units

instance Units.Coercion (VectorSurfaceFunction2D units1) (VectorSurfaceFunction2D units2)

instance
  units1 ~ units2 =>
  Addition
    (VectorSurfaceFunction2D units1)
    (VectorSurfaceFunction2D units2)
    (VectorSurfaceFunction2D units1)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction1D units1)
    (VectorSurfaceFunction2D units2)
    (VectorSurfaceFunction2D units3)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorSurfaceFunction2D units1)
    (SurfaceFunction1D units2)
    (VectorSurfaceFunction2D units3)

instance
  Multiplication_
    (SurfaceFunction1D units1)
    (VectorSurfaceFunction2D units2)
    (VectorSurfaceFunction2D (units1 ?*? units2))

instance
  Multiplication_
    (VectorSurfaceFunction2D units1)
    (SurfaceFunction1D units2)
    (VectorSurfaceFunction2D (units1 ?*? units2))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Quantity units1)
    (VectorSurfaceFunction2D units2)
    (VectorSurfaceFunction2D units3)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorSurfaceFunction2D units1)
    (Quantity units2)
    (VectorSurfaceFunction2D units3)

instance
  DotMultiplication_
    (VectorSurfaceFunction2D units1)
    (VectorSurfaceFunction2D units2)
    (SurfaceFunction1D (units1 ?*? units2))

instance
  CrossMultiplication_
    (VectorSurfaceFunction2D units1)
    (VectorSurfaceFunction2D units2)
    (SurfaceFunction1D (units1 ?*? units2))

new ::
  Compiled units ->
  (SurfaceParameter -> VectorSurfaceFunction2D units) ->
  VectorSurfaceFunction2D units
derivative :: SurfaceParameter -> VectorSurfaceFunction2D units -> VectorSurfaceFunction2D units
constant :: Vector2D units -> VectorSurfaceFunction2D units
xComponent :: VectorSurfaceFunction2D units -> SurfaceFunction1D units
yComponent :: VectorSurfaceFunction2D units -> SurfaceFunction1D units
components :: VectorSurfaceFunction2D units -> (SurfaceFunction1D units, SurfaceFunction1D units)
squaredMagnitude_ :: VectorSurfaceFunction2D units -> SurfaceFunction1D (units ?*? units)
