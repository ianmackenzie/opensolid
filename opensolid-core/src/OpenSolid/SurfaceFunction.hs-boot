module OpenSolid.SurfaceFunction
  ( SurfaceFunction
  , Compiled
  , DivisionByZero
  , evaluate
  , evaluateBounds
  , derivative
  , quotient
  , quotient'
  , unsafeQuotient
  , unsafeQuotient'
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)

type role SurfaceFunction nominal

type SurfaceFunction :: Type -> Type
data SurfaceFunction units

instance HasField "du" (SurfaceFunction units) (SurfaceFunction units)

instance HasField "dv" (SurfaceFunction units) (SurfaceFunction units)

type Compiled units = CompiledFunction UvPoint (Qty units) UvBounds (Bounds units)

instance HasField "compiled" (SurfaceFunction units) (Compiled units)

instance Negation (SurfaceFunction units)

data DivisionByZero

evaluate :: SurfaceFunction units -> UvPoint -> Qty units
evaluateBounds :: SurfaceFunction units -> UvBounds -> Bounds units
derivative :: SurfaceParameter -> SurfaceFunction units -> SurfaceFunction units
quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  SurfaceFunction units1 ->
  SurfaceFunction units2 ->
  Result DivisionByZero (SurfaceFunction units3)
quotient' ::
  Tolerance units2 =>
  SurfaceFunction units1 ->
  SurfaceFunction units2 ->
  Result DivisionByZero (SurfaceFunction (units1 :/: units2))
unsafeQuotient ::
  Units.Quotient units1 units2 units3 =>
  SurfaceFunction units1 ->
  SurfaceFunction units2 ->
  SurfaceFunction units3
unsafeQuotient' ::
  SurfaceFunction units1 ->
  SurfaceFunction units2 ->
  SurfaceFunction (units1 :/: units2)
