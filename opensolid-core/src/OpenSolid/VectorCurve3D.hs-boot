module OpenSolid.VectorCurve3D
  ( VectorCurve3D
  , Compiled
  , constant
  , new
  , on
  , bezier
  , compiled
  , derivative
  , isZero
  , evaluate
  , evaluateBounds
  , quotient
  , quotient_
  , squaredMagnitude_
  , normalize
  , magnitude
  , transformBy
  , desingularized
  )
where

import {-# SOURCE #-} OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import {-# SOURCE #-} OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.DivisionByZero (DivisionByZero)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Primitives (Plane3D, Transform3D, Vector3D, VectorBounds3D)
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)

type role VectorCurve3D nominal nominal

type VectorCurve3D :: Type -> Type -> Type
data VectorCurve3D units space

type Compiled units space =
  CompiledFunction
    Number
    (Vector3D units space)
    (Interval Unitless)
    (VectorBounds3D units space)

instance HasUnits (VectorCurve3D units space) units

instance Composition (VectorCurve3D units space) (Curve1D Unitless) (VectorCurve3D units space)

instance Negation (VectorCurve3D units space)

instance Multiplication Sign (VectorCurve3D units space) (VectorCurve3D units space)

instance Multiplication (VectorCurve3D units space) Sign (VectorCurve3D units space)

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve3D unitsA space1) (VectorCurve3D unitsB space2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (VectorCurve3D units1 space1)

instance
  Multiplication_
    (Curve1D units1)
    (VectorCurve3D units2 space)
    (VectorCurve3D (units1 ?*? units2) space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1D units1) (VectorCurve3D units2 space) (VectorCurve3D units3 space)

instance
  Multiplication_
    (VectorCurve3D units1 space)
    (Curve1D units2)
    (VectorCurve3D (units1 ?*? units2) space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3D units1 space) (Curve1D units2) (VectorCurve3D units3 space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (VectorCurve3D units2 space) (VectorCurve3D units3 space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3D units1 space) (Quantity units2) (VectorCurve3D units3 space)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve3D units1 space) (Curve1D.WithNoZeros units2) (VectorCurve3D units3 space)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (VectorCurve3D units1 space)
    (Curve1D.WithNoInteriorZeros units2)
    (VectorCurve3D units3 space)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (Curve1D (units1 ?*? units2))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (Curve1D units3)

constant :: Vector3D units space -> VectorCurve3D units space
new :: Compiled units space -> VectorCurve3D units space -> VectorCurve3D units space
on :: Plane3D global local -> VectorCurve2D units local -> VectorCurve3D units global
bezier :: NonEmpty (Vector3D units space) -> VectorCurve3D units space
compiled :: VectorCurve3D units space -> Compiled units space
derivative :: VectorCurve3D units space -> VectorCurve3D units space
isZero :: Tolerance units => VectorCurve3D units space -> Bool
evaluate :: VectorCurve3D units space -> Number -> Vector3D units space
evaluateBounds :: VectorCurve3D units space -> Interval Unitless -> VectorBounds3D units space
quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorCurve3D units1 space ->
  Curve1D units2 ->
  Result DivisionByZero (VectorCurve3D units3 space)
quotient_ ::
  Tolerance units2 =>
  VectorCurve3D units1 space ->
  Curve1D units2 ->
  Result DivisionByZero (VectorCurve3D (units1 ?/? units2) space)
squaredMagnitude_ :: VectorCurve3D units space -> Curve1D (units ?*? units)
normalize :: Tolerance units => VectorCurve3D units space -> VectorCurve3D Unitless space
magnitude :: Tolerance units => VectorCurve3D units space -> Curve1D units
transformBy :: Transform3D tag space -> VectorCurve3D units space -> VectorCurve3D units space
desingularized ::
  VectorCurve3D units space ->
  VectorCurve3D units space ->
  VectorCurve3D units space ->
  VectorCurve3D units space
