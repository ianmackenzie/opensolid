module OpenSolid.Curve1D
  ( Curve1D
  , Compiled
  , Zero
  , IsZero (IsZero)
  , WithNoInteriorZeros (WithNoInteriorZeros)
  , WithNoZeros (WithNoZeros)
  , new
  , constant
  , bezier
  , compiled
  , derivative
  , evaluate
  , evaluateBounds
  , zeros
  , singularityTolerance
  , desingularized
  , squared_
  , sqrt_
  )
where

import {-# SOURCE #-} OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve1D.Zero (Zero)
import OpenSolid.FFI (FFI)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units

type role Curve1D nominal

type Curve1D :: Type -> Type
data Curve1D units

data IsZero = IsZero

instance Composition (Curve1D units) (Curve1D Unitless) (Curve1D units)

type Compiled units = CompiledFunction Number (Quantity units) (Interval Unitless) (Interval units)

instance FFI (Curve1D Unitless)

instance HasUnits (Curve1D units) units

instance Units.Coercion (Curve1D units1) (Curve1D units2)

newtype WithNoZeros units = WithNoZeros (Curve1D units)

newtype WithNoInteriorZeros units = WithNoInteriorZeros (Curve1D units)

instance ApproximateEquality (Curve1D units) units

instance units1 ~ units2 => Addition (Curve1D units1) (Curve1D units2) (Curve1D units1)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1D units1) (Quantity units2) (Curve1D units3)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (Curve1D units2) (Curve1D units3)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1D units1) (Curve1D units2) (Curve1D units3)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Curve1D units1) (WithNoZeros units2) (Curve1D units3)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Curve1D units1) (WithNoInteriorZeros units2) (Curve1D units3)

new :: Compiled units -> Curve1D units -> Curve1D units
constant :: Quantity units -> Curve1D units
bezier :: NonEmpty (Quantity units) -> Curve1D units
compiled :: Curve1D units -> Compiled units
derivative :: Curve1D units -> Curve1D units
evaluate :: Curve1D units -> Number -> Quantity units
evaluateBounds :: Curve1D units -> Interval Unitless -> Interval units
zeros :: Tolerance units => Curve1D units -> Result IsZero (List Zero)
singularityTolerance :: Curve1D units -> Quantity units
desingularized :: Curve1D units -> Curve1D units -> Curve1D units -> Curve1D units
squared_ :: Curve1D units -> Curve1D (units ?*? units)
sqrt_ :: Tolerance units => Curve1D (units ?*? units) -> Curve1D units
