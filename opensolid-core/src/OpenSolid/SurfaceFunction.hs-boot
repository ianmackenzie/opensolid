module OpenSolid.SurfaceFunction
  ( SurfaceFunction
  , Compiled
  , constant
  , u
  , v
  , evaluate
  , evaluateBounds
  , derivative
  , squared
  , squared'
  , cubed
  , quotient
  , quotient'
  , unsafeQuotient
  , unsafeQuotient'
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve (Curve)
import OpenSolid.DivisionByZero (DivisionByZero)
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

instance Composition (SurfaceFunction Unitless) (Curve units) (SurfaceFunction units)

instance units1 ~ units2 => ApproximateEquality (SurfaceFunction units1) (SurfaceFunction units2) units1

instance units1 ~ units2 => ApproximateEquality (SurfaceFunction units1) (Qty units2) units1

instance Negation (SurfaceFunction units)

instance
  units1 ~ units2 =>
  Addition
    (SurfaceFunction units1)
    (SurfaceFunction units2)
    (SurfaceFunction units1)

instance
  units1 ~ units2 =>
  Addition
    (SurfaceFunction units1)
    (Qty units2)
    (SurfaceFunction units1)

instance
  units1 ~ units2 =>
  Addition
    (Qty units1)
    (SurfaceFunction units2)
    (SurfaceFunction units1)

instance
  units1 ~ units2 =>
  Subtraction
    (SurfaceFunction units1)
    (SurfaceFunction units2)
    (SurfaceFunction units1)

instance
  units1 ~ units2 =>
  Subtraction
    (SurfaceFunction units1)
    (Qty units2)
    (SurfaceFunction units1)

instance
  units1 ~ units2 =>
  Subtraction
    (Qty units1)
    (SurfaceFunction units2)
    (SurfaceFunction units1)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (SurfaceFunction units1) (SurfaceFunction units2) (SurfaceFunction units3)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (SurfaceFunction units1) (Qty units2) (SurfaceFunction units3)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (SurfaceFunction units2) (SurfaceFunction units3)

instance
  Units.Quotient units1 units2 units3 =>
  Division (SurfaceFunction units1) (Qty units2) (SurfaceFunction units3)

constant :: Qty units -> SurfaceFunction units
u :: SurfaceFunction Unitless
v :: SurfaceFunction Unitless
evaluate :: SurfaceFunction units -> UvPoint -> Qty units
evaluateBounds :: SurfaceFunction units -> UvBounds -> Bounds units
derivative :: SurfaceParameter -> SurfaceFunction units -> SurfaceFunction units
squared :: Units.Squared units1 units2 => SurfaceFunction units1 -> SurfaceFunction units2
squared' :: SurfaceFunction units1 -> SurfaceFunction (units1 :*: units1)
cubed :: SurfaceFunction Unitless -> SurfaceFunction Unitless
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
