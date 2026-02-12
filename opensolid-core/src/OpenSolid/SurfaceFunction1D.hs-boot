module OpenSolid.SurfaceFunction1D
  ( SurfaceFunction1D (compiled, du, dv)
  , Compiled
  , WithNoInteriorZeros (WithNoInteriorZeros)
  , WithNoZeros (WithNoZeros)
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

import {-# SOURCE #-} OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import OpenSolid.DivisionByZero (DivisionByZero)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.UvBounds (UvBounds)
import {-# SOURCE #-} OpenSolid.UvPoint (UvPoint)

type role SurfaceFunction1D nominal

data SurfaceFunction1D units = SurfaceFunction1D
  { compiled :: Compiled units
  , du :: ~(SurfaceFunction1D units)
  , dv :: ~(SurfaceFunction1D units)
  }

type Compiled units = CompiledFunction UvPoint (Quantity units) UvBounds (Interval units)

newtype WithNoZeros units = WithNoZeros (SurfaceFunction1D units)

newtype WithNoInteriorZeros units = WithNoInteriorZeros (SurfaceFunction1D units)

instance Composition (Curve1D units) (SurfaceFunction1D Unitless) (SurfaceFunction1D units)

instance ApproximateEquality (SurfaceFunction1D units) units

instance Negation (SurfaceFunction1D units)

instance
  units1 ~ units2 =>
  Addition
    (SurfaceFunction1D units1)
    (SurfaceFunction1D units2)
    (SurfaceFunction1D units1)

instance
  units1 ~ units2 =>
  Addition
    (SurfaceFunction1D units1)
    (Quantity units2)
    (SurfaceFunction1D units1)

instance
  units1 ~ units2 =>
  Addition
    (Quantity units1)
    (SurfaceFunction1D units2)
    (SurfaceFunction1D units1)

instance
  units1 ~ units2 =>
  Subtraction
    (SurfaceFunction1D units1)
    (SurfaceFunction1D units2)
    (SurfaceFunction1D units1)

instance
  units1 ~ units2 =>
  Subtraction
    (SurfaceFunction1D units1)
    (Quantity units2)
    (SurfaceFunction1D units1)

instance
  units1 ~ units2 =>
  Subtraction
    (Quantity units1)
    (SurfaceFunction1D units2)
    (SurfaceFunction1D units1)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (SurfaceFunction1D units1) (SurfaceFunction1D units2) (SurfaceFunction1D units3)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (SurfaceFunction1D units1) (Quantity units2) (SurfaceFunction1D units3)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (SurfaceFunction1D units2) (SurfaceFunction1D units3)

instance
  Units.Quotient units1 units2 units3 =>
  Division (SurfaceFunction1D units1) (Quantity units2) (SurfaceFunction1D units3)

constant :: Quantity units -> SurfaceFunction1D units
zero :: SurfaceFunction1D units
u :: SurfaceFunction1D Unitless
v :: SurfaceFunction1D Unitless
parameter :: SurfaceParameter -> SurfaceFunction1D Unitless
evaluate :: SurfaceFunction1D units -> UvPoint -> Quantity units
evaluateBounds :: SurfaceFunction1D units -> UvBounds -> Interval units
derivative :: SurfaceParameter -> SurfaceFunction1D units -> SurfaceFunction1D units
squared :: Units.Squared units1 units2 => SurfaceFunction1D units1 -> SurfaceFunction1D units2
squared_ :: SurfaceFunction1D units1 -> SurfaceFunction1D (units1 ?*? units1)
cubed :: SurfaceFunction1D Unitless -> SurfaceFunction1D Unitless
quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  SurfaceFunction1D units1 ->
  SurfaceFunction1D units2 ->
  Result DivisionByZero (SurfaceFunction1D units3)
quotient_ ::
  Tolerance units2 =>
  SurfaceFunction1D units1 ->
  SurfaceFunction1D units2 ->
  Result DivisionByZero (SurfaceFunction1D (units1 ?/? units2))
unsafeQuotient ::
  Units.Quotient units1 units2 units3 =>
  SurfaceFunction1D units1 ->
  SurfaceFunction1D units2 ->
  SurfaceFunction1D units3
unsafeQuotient_ ::
  SurfaceFunction1D units1 ->
  SurfaceFunction1D units2 ->
  SurfaceFunction1D (units1 ?/? units2)
