module OpenSolid.SurfaceFunction
  ( SurfaceFunction
  , Compiled
  , evaluate
  , evaluateBounds
  , derivative
  , new
  , constant
  , squared'
  , sqrt'
  , quotient
  , quotient'
  , sin
  , cos
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

instance Units.Coercion (SurfaceFunction units1) (SurfaceFunction units2)

instance HasField "du" (SurfaceFunction units) (SurfaceFunction units)

instance HasField "dv" (SurfaceFunction units) (SurfaceFunction units)

type Compiled units = CompiledFunction UvPoint (Qty units) UvBounds (Bounds units)

instance HasField "compiled" (SurfaceFunction units) (Compiled units)

instance Negation (SurfaceFunction units)

instance
  units1 ~ units2 =>
  Addition (SurfaceFunction units1) (SurfaceFunction units2) (SurfaceFunction units1)

instance
  units1 ~ units2 =>
  Subtraction (SurfaceFunction units1) (SurfaceFunction units2) (SurfaceFunction units1)

instance
  Multiplication'
    (SurfaceFunction units1)
    (SurfaceFunction units2)
    (SurfaceFunction (units1 :*: units2))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (SurfaceFunction units1) (SurfaceFunction units2) (SurfaceFunction units3)

evaluate :: SurfaceFunction units -> UvPoint -> Qty units
evaluateBounds :: SurfaceFunction units -> UvBounds -> Bounds units
derivative :: SurfaceParameter -> SurfaceFunction units -> SurfaceFunction units
new :: Compiled units -> (SurfaceParameter -> SurfaceFunction units) -> SurfaceFunction units
constant :: Qty units -> SurfaceFunction units
squared' :: SurfaceFunction units -> SurfaceFunction (units :*: units)
sqrt' :: Tolerance units => SurfaceFunction (units :*: units) -> SurfaceFunction units
quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  SurfaceFunction units1 ->
  SurfaceFunction units2 ->
  SurfaceFunction units3
quotient' ::
  Tolerance units2 =>
  SurfaceFunction units1 ->
  SurfaceFunction units2 ->
  SurfaceFunction (units1 :/: units2)
sin :: SurfaceFunction Radians -> SurfaceFunction Unitless
cos :: SurfaceFunction Radians -> SurfaceFunction Unitless
