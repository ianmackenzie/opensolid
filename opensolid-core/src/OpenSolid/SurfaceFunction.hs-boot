module OpenSolid.SurfaceFunction
  ( SurfaceFunction (compiled, du, dv)
  , Compiled
  , constant
  , zero
  , u
  , v
  , parameter
  , evaluate
  , evaluateBounds
  , derivative
  , squared
  , squared_
  , cubed
  , quotient
  , quotient_
  , unsafeQuotient
  , unsafeQuotient_
  )
where

import OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve (Curve)
import OpenSolid.DivisionByZero (DivisionByZero)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)

type role SurfaceFunction nominal

data SurfaceFunction units = SurfaceFunction
  { compiled :: Compiled units
  , du :: ~(SurfaceFunction units)
  , dv :: ~(SurfaceFunction units)
  }

type Compiled units = CompiledFunction UvPoint (Quantity units) UvBounds (Interval units)

instance Composition (SurfaceFunction Unitless) (Curve units) (SurfaceFunction units)

instance ApproximateEquality (SurfaceFunction units) units

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
    (Quantity units2)
    (SurfaceFunction units1)

instance
  units1 ~ units2 =>
  Addition
    (Quantity units1)
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
    (Quantity units2)
    (SurfaceFunction units1)

instance
  units1 ~ units2 =>
  Subtraction
    (Quantity units1)
    (SurfaceFunction units2)
    (SurfaceFunction units1)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (SurfaceFunction units1) (SurfaceFunction units2) (SurfaceFunction units3)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (SurfaceFunction units1) (Quantity units2) (SurfaceFunction units3)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (SurfaceFunction units2) (SurfaceFunction units3)

instance
  Units.Quotient units1 units2 units3 =>
  Division (SurfaceFunction units1) (Quantity units2) (SurfaceFunction units3)

constant :: Quantity units -> SurfaceFunction units
zero :: SurfaceFunction units
u :: SurfaceFunction Unitless
v :: SurfaceFunction Unitless
parameter :: SurfaceParameter -> SurfaceFunction Unitless
evaluate :: SurfaceFunction units -> UvPoint -> Quantity units
evaluateBounds :: SurfaceFunction units -> UvBounds -> Interval units
derivative :: SurfaceParameter -> SurfaceFunction units -> SurfaceFunction units
squared :: Units.Squared units1 units2 => SurfaceFunction units1 -> SurfaceFunction units2
squared_ :: SurfaceFunction units1 -> SurfaceFunction (units1 ?*? units1)
cubed :: SurfaceFunction Unitless -> SurfaceFunction Unitless
quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  SurfaceFunction units1 ->
  SurfaceFunction units2 ->
  Result DivisionByZero (SurfaceFunction units3)
quotient_ ::
  Tolerance units2 =>
  SurfaceFunction units1 ->
  SurfaceFunction units2 ->
  Result DivisionByZero (SurfaceFunction (units1 ?/? units2))
unsafeQuotient ::
  Units.Quotient units1 units2 units3 =>
  SurfaceFunction units1 ->
  SurfaceFunction units2 ->
  SurfaceFunction units3
unsafeQuotient_ ::
  SurfaceFunction units1 ->
  SurfaceFunction units2 ->
  SurfaceFunction (units1 ?/? units2)
