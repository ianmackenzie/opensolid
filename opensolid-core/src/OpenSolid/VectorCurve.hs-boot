module OpenSolid.VectorCurve
  ( VectorCurve
  , Exists
  , Compiled
  , constant
  , singular0
  , singular1
  , value
  , bounds
  , derivative
  , derivativeValue
  , squaredMagnitude_
  )
where

import GHC.TypeLits (Natural)
import OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import OpenSolid.Interval (Interval)
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Nonzero (Nonzero)
import OpenSolid.Prelude
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector (Vector)
import OpenSolid.VectorBounds (VectorBounds)

type role VectorCurve nominal nominal nominal

data VectorCurve (dimension :: Natural) (units :: Type) (space :: Type)

class Exists (dimension :: Natural) (units :: Type) (space :: Type)

type Compiled dimension units space =
  CompiledFunction
    Number
    (Vector dimension units space)
    (Interval Unitless)
    (VectorBounds dimension units space)

instance HasUnits (VectorCurve dimension units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve 2 units1 space1) (VectorCurve 2 units2 space2)

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve 3 units1 space1) (VectorCurve 3 units2 space2)

instance HasUnits (Nondegenerate (VectorCurve dimension units space)) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (Nondegenerate (VectorCurve 2 units1 space1))
    (Nondegenerate (VectorCurve 2 units2 space2))

instance
  space1 ~ space2 =>
  Units.Coercion
    (Nondegenerate (VectorCurve 3 units1 space1))
    (Nondegenerate (VectorCurve 3 units2 space2))

instance
  Exists dimension units space =>
  Composition
    (VectorCurve dimension units space)
    (Curve1D Unitless)
    (VectorCurve dimension units space)

instance Exists dimension units space => Negation (VectorCurve dimension units space)

instance
  Exists dimension units space =>
  Multiplication Sign (VectorCurve dimension units space) (VectorCurve dimension units space)

instance
  Exists dimension units space =>
  Multiplication (VectorCurve dimension units space) Sign (VectorCurve dimension units space)

instance
  ( Exists dimension1 units1 space1
  , dimension1 ~ dimension2
  , space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (VectorCurve dimension1 units1 space1)
    (VectorCurve dimension2 units2 space2)
    (VectorCurve dimension1 units1 space1)

instance
  ( Exists dimension1 units1 space1
  , dimension1 ~ dimension2
  , space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (VectorCurve dimension1 units1 space1)
    (VectorCurve dimension2 units2 space2)
    (VectorCurve dimension1 units1 space1)

instance
  Multiplication_
    (Curve1D units1)
    (VectorCurve 2 units2 space)
    (VectorCurve 2 (units1 ?*? units2) space)

instance
  Multiplication_
    (Curve1D units1)
    (VectorCurve 3 units2 space)
    (VectorCurve 3 (units1 ?*? units2) space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1D units1) (VectorCurve 2 units2 space) (VectorCurve 2 units3 space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1D units1) (VectorCurve 3 units2 space) (VectorCurve 3 units3 space)

instance
  Multiplication_
    (VectorCurve 2 units1 space)
    (Curve1D units2)
    (VectorCurve 2 (units1 ?*? units2) space)

instance
  Multiplication_
    (VectorCurve 3 units1 space)
    (Curve1D units2)
    (VectorCurve 3 (units1 ?*? units2) space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve 2 units1 space) (Curve1D units2) (VectorCurve 2 units3 space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve 3 units1 space) (Curve1D units2) (VectorCurve 3 units3 space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (VectorCurve 2 units2 space) (VectorCurve 2 units3 space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (VectorCurve 3 units2 space) (VectorCurve 3 units3 space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve 2 units1 space) (Quantity units2) (VectorCurve 2 units3 space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve 3 units1 space) (Quantity units2) (VectorCurve 3 units3 space)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve 2 units1 space) (Nonzero (Curve1D units2)) (VectorCurve 2 units3 space)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve 3 units1 space) (Nonzero (Curve1D units2)) (VectorCurve 3 units3 space)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (VectorCurve 2 units1 space)
    (Nondegenerate (Curve1D units2))
    (VectorCurve 2 units3 space)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (VectorCurve 3 units1 space)
    (Nondegenerate (Curve1D units2))
    (VectorCurve 3 units3 space)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve 2 units1 space1)
    (VectorCurve 2 units2 space2)
    (Curve1D (units1 ?*? units2))

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve 3 units1 space1)
    (VectorCurve 3 units2 space2)
    (Curve1D (units1 ?*? units2))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve 2 units1 space1)
    (VectorCurve 2 units2 space2)
    (Curve1D units3)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve 3 units1 space1)
    (VectorCurve 3 units2 space2)
    (Curve1D units3)

instance Exists 2 units space

instance Exists 3 units space

constant ::
  Exists dimension units space =>
  Vector dimension units space ->
  VectorCurve dimension units space
singular0 :: Exists dimension units space => VectorCurve dimension units space -> Bool
singular1 :: Exists dimension units space => VectorCurve dimension units space -> Bool
value ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Number ->
  Vector dimension units space
bounds ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
derivative ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  VectorCurve dimension units space
derivativeValue ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Number ->
  Vector dimension units space
squaredMagnitude_ ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Curve1D (units ?*? units)
