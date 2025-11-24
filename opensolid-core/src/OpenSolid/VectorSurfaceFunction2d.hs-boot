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
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.VectorBounds2d (VectorBounds2d)

type role VectorSurfaceFunction2d nominal nominal

type VectorSurfaceFunction2d :: Type -> Type -> Type
data VectorSurfaceFunction2d space units

type Compiled space units =
  CompiledFunction
    UvPoint
    (Vector2d space units)
    UvBounds
    (VectorBounds2d space units)

instance
  HasField
    "compiled"
    (VectorSurfaceFunction2d space units)
    (Compiled space units)

instance HasUnits (VectorSurfaceFunction2d space units) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (VectorSurfaceFunction2d space1 units1)
    (VectorSurfaceFunction2d space2 units2)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction units1)
    (VectorSurfaceFunction2d space units2)
    (VectorSurfaceFunction2d space units3)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorSurfaceFunction2d space units1)
    (SurfaceFunction units2)
    (VectorSurfaceFunction2d space units3)

instance
  Multiplication_
    (SurfaceFunction units1)
    (VectorSurfaceFunction2d space units2)
    (VectorSurfaceFunction2d space (units1 ?*? units2))

instance
  Multiplication_
    (VectorSurfaceFunction2d space units1)
    (SurfaceFunction units2)
    (VectorSurfaceFunction2d space (units1 ?*? units2))

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorSurfaceFunction2d space1 units1)
    (VectorSurfaceFunction2d space2 units2)
    (SurfaceFunction (units1 ?*? units2))

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorSurfaceFunction2d space1 units1)
    (VectorSurfaceFunction2d space2 units2)
    (SurfaceFunction (units1 ?*? units2))

new ::
  Compiled space units ->
  (SurfaceParameter -> VectorSurfaceFunction2d space units) ->
  VectorSurfaceFunction2d space units
derivative ::
  SurfaceParameter ->
  VectorSurfaceFunction2d space units ->
  VectorSurfaceFunction2d space units
constant :: Vector2d space units -> VectorSurfaceFunction2d space units
xComponent :: VectorSurfaceFunction2d space units -> SurfaceFunction units
yComponent :: VectorSurfaceFunction2d space units -> SurfaceFunction units
components ::
  VectorSurfaceFunction2d space units ->
  (SurfaceFunction units, SurfaceFunction units)
squaredMagnitude_ :: VectorSurfaceFunction2d space units -> SurfaceFunction (units ?*? units)
