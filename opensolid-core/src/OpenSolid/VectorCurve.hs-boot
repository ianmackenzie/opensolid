module OpenSolid.VectorCurve
  ( VectorCurve
  , VectorCurve2D
  , VectorCurve3D
  , VectorCurveExists
  , Compiled
  , constant
  , hasDegenerateStart
  , hasDegenerateEnd
  , valueAt
  , valueOf
  , range
  , derivative
  , derivativeAt
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
import OpenSolid.Units (Units)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector (Vector)
import OpenSolid.VectorBounds (VectorBounds)

type role VectorCurve nominal nominal nominal

data VectorCurve (dimension :: Natural) (units :: Type) (space :: Type)

type VectorCurve2D units = VectorCurve 2 units Void

type VectorCurve3D units space = VectorCurve 3 units space

class VectorCurveExists (dimension :: Natural) (units :: Type) (space :: Type)

type Compiled dimension units space =
  CompiledFunction
    Number
    (Vector dimension units space)
    (Interval Unitless)
    (VectorBounds dimension units space)

instance Units (VectorCurve dimension units space) units

instance Units.Coercion (VectorCurve2D units1) (VectorCurve2D units2)

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve3D units1 space1) (VectorCurve3D units2 space2)

instance Units (Nondegenerate (VectorCurve dimension units space)) units

instance
  Units.Coercion
    (Nondegenerate (VectorCurve2D units1))
    (Nondegenerate (VectorCurve2D units2))

instance
  space1 ~ space2 =>
  Units.Coercion
    (Nondegenerate (VectorCurve3D units1 space1))
    (Nondegenerate (VectorCurve3D units2 space2))

instance
  VectorCurveExists dimension units space =>
  Composition
    (VectorCurve dimension units space)
    (Curve1D Unitless)
    (VectorCurve dimension units space)

instance VectorCurveExists dimension units space => Negation (VectorCurve dimension units space)

instance
  VectorCurveExists dimension units space =>
  Multiplication Sign (VectorCurve dimension units space) (VectorCurve dimension units space)

instance
  VectorCurveExists dimension units space =>
  Multiplication (VectorCurve dimension units space) Sign (VectorCurve dimension units space)

instance
  ( VectorCurveExists dimension1 units1 space1
  , dimension1 ~ dimension2
  , space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (VectorCurve dimension1 units1 space1)
    (VectorCurve dimension2 units2 space2)
    (VectorCurve dimension1 units1 space1)

instance
  ( VectorCurveExists dimension1 units1 space1
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
    (VectorCurve2D units2)
    (VectorCurve2D (units1 ?*? units2))

instance
  Multiplication_
    (Curve1D units1)
    (VectorCurve3D units2 space)
    (VectorCurve3D (units1 ?*? units2) space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1D units1) (VectorCurve2D units2) (VectorCurve2D units3)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1D units1) (VectorCurve3D units2 space) (VectorCurve3D units3 space)

instance
  Multiplication_
    (VectorCurve2D units1)
    (Curve1D units2)
    (VectorCurve2D (units1 ?*? units2))

instance
  Multiplication_
    (VectorCurve3D units1 space)
    (Curve1D units2)
    (VectorCurve3D (units1 ?*? units2) space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve2D units1) (Curve1D units2) (VectorCurve2D units3)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3D units1 space) (Curve1D units2) (VectorCurve3D units3 space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (VectorCurve2D units2) (VectorCurve2D units3)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (VectorCurve3D units2 space) (VectorCurve3D units3 space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve2D units1) (Quantity units2) (VectorCurve2D units3)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3D units1 space) (Quantity units2) (VectorCurve3D units3 space)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve2D units1) (Nonzero (Curve1D units2)) (VectorCurve2D units3)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve3D units1 space) (Nonzero (Curve1D units2)) (VectorCurve3D units3 space)

instance
  DotMultiplication_
    (VectorCurve2D units1)
    (VectorCurve2D units2)
    (Curve1D (units1 ?*? units2))

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (Curve1D (units1 ?*? units2))

instance
  Units.Product units1 units2 units3 =>
  DotMultiplication
    (VectorCurve2D units1)
    (VectorCurve2D units2)
    (Curve1D units3)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (Curve1D units3)

instance VectorCurveExists 2 units Void

instance VectorCurveExists 3 units space

constant ::
  VectorCurveExists dimension units space =>
  Vector dimension units space ->
  VectorCurve dimension units space
hasDegenerateStart ::
  VectorCurveExists dimension units space =>
  VectorCurve dimension units space ->
  Bool
hasDegenerateEnd ::
  VectorCurveExists dimension units space =>
  VectorCurve dimension units space ->
  Bool
valueAt ::
  VectorCurveExists dimension units space =>
  Number ->
  VectorCurve dimension units space ->
  Vector dimension units space
valueOf ::
  VectorCurveExists dimension units space =>
  VectorCurve dimension units space ->
  Number ->
  Vector dimension units space
range ::
  VectorCurveExists dimension units space =>
  Interval Unitless ->
  VectorCurve dimension units space ->
  VectorBounds dimension units space
derivative ::
  VectorCurveExists dimension units space =>
  VectorCurve dimension units space ->
  VectorCurve dimension units space
derivativeAt ::
  VectorCurveExists dimension units space =>
  Number ->
  VectorCurve dimension units space ->
  Vector dimension units space
squaredMagnitude_ ::
  VectorCurveExists dimension units space =>
  VectorCurve dimension units space ->
  Curve1D (units ?*? units)
