module OpenSolid.SurfaceFunction1D
  ( SurfaceFunction1D
  , Compiled
  , constant
  , zero
  , u
  , v
  , valueAt
  , valueOf
  , range
  , compiled
  , partialDerivatives
  , partialDerivativesAt
  , partialDerivativeRanges
  , secondPartialDerivatives
  , secondPartialDerivativesAt
  , secondPartialDerivativeRanges
  , squared
  , squared_
  , cubed
  )
where

import OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)

type role SurfaceFunction1D nominal

data SurfaceFunction1D (units :: Type)

type Compiled units = CompiledFunction UvPoint (Quantity units) UvBounds (Interval units)

instance Composition (Curve1D units) (SurfaceFunction1D Unitless) (SurfaceFunction1D units)

instance ApproximateEquality (SurfaceFunction1D units) (Tolerance units)

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
valueAt :: UvPoint -> SurfaceFunction1D units -> Quantity units
valueOf :: SurfaceFunction1D units -> UvPoint -> Quantity units
range :: UvBounds -> SurfaceFunction1D units -> Interval units
compiled :: SurfaceFunction1D units -> Compiled units
partialDerivatives :: SurfaceFunction1D units -> (SurfaceFunction1D units, SurfaceFunction1D units)
partialDerivativesAt :: UvPoint -> SurfaceFunction1D units -> (Quantity units, Quantity units)
partialDerivativeRanges :: UvBounds -> SurfaceFunction1D units -> (Interval units, Interval units)
secondPartialDerivatives ::
  SurfaceFunction1D units ->
  (SurfaceFunction1D units, SurfaceFunction1D units, SurfaceFunction1D units)
secondPartialDerivativesAt ::
  UvPoint ->
  SurfaceFunction1D units ->
  (Quantity units, Quantity units, Quantity units)
secondPartialDerivativeRanges ::
  UvBounds ->
  SurfaceFunction1D units ->
  (Interval units, Interval units, Interval units)
squared :: Units.Squared units1 units2 => SurfaceFunction1D units1 -> SurfaceFunction1D units2
squared_ :: SurfaceFunction1D units1 -> SurfaceFunction1D (units1 ?*? units1)
cubed :: SurfaceFunction1D Unitless -> SurfaceFunction1D Unitless
