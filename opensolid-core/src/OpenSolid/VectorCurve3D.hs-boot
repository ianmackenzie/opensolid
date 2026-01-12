module OpenSolid.VectorCurve3D
  ( VectorCurve3D
  , Compiled
  , constant
  , new
  , on
  , evaluate
  , evaluateBounds
  , quotient
  , quotient_
  , unsafeQuotient
  , unsafeQuotient_
  , magnitude
  , transformBy
  )
where

import GHC.Records (HasField)
import OpenSolid.Bounds (Bounds)
import OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve (Curve)
import OpenSolid.DivisionByZero (DivisionByZero)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Prelude
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.VectorBounds3D (VectorBounds3D)
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)

type role VectorCurve3D nominal nominal

type VectorCurve3D :: Type -> Type -> Type
data VectorCurve3D units space

instance HasField "compiled" (VectorCurve3D units space) (Compiled units space)

instance HasField "derivative" (VectorCurve3D units space) (VectorCurve3D units space)

type Compiled units space =
  CompiledFunction
    Number
    (Vector3D units space)
    (Bounds Unitless)
    (VectorBounds3D units space)

instance HasUnits (VectorCurve3D units space) units

instance Negation (VectorCurve3D units space)

instance Multiplication Sign (VectorCurve3D units space) (VectorCurve3D units space)

instance Multiplication (VectorCurve3D units space) Sign (VectorCurve3D units space)

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve3D unitsA space1) (VectorCurve3D unitsB space2)

instance
  Multiplication_
    (Curve units1)
    (VectorCurve3D units2 space)
    (VectorCurve3D (units1 ?*? units2) space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (VectorCurve3D units2 space) (VectorCurve3D units3 space)

instance
  Multiplication_
    (VectorCurve3D units1 space)
    (Curve units2)
    (VectorCurve3D (units1 ?*? units2) space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3D units1 space) (Curve units2) (VectorCurve3D units3 space)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (Curve (units1 ?*? units2))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (Curve units3)

constant :: Vector3D units space -> VectorCurve3D units space
new :: Compiled units space -> VectorCurve3D units space -> VectorCurve3D units space
on :: Plane3D global local -> VectorCurve2D units local -> VectorCurve3D units global
evaluate :: VectorCurve3D units space -> Number -> Vector3D units space
evaluateBounds :: VectorCurve3D units space -> Bounds Unitless -> VectorBounds3D units space
quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorCurve3D units1 space ->
  Curve units2 ->
  Result DivisionByZero (VectorCurve3D units3 space)
quotient_ ::
  Tolerance units2 =>
  VectorCurve3D units1 space ->
  Curve units2 ->
  Result DivisionByZero (VectorCurve3D (units1 ?/? units2) space)
unsafeQuotient ::
  Units.Quotient units1 units2 units3 =>
  VectorCurve3D units1 space ->
  Curve units2 ->
  VectorCurve3D units3 space
unsafeQuotient_ ::
  VectorCurve3D units1 space ->
  Curve units2 ->
  VectorCurve3D (units1 ?/? units2) space
magnitude :: Tolerance units => VectorCurve3D units space -> Curve units
transformBy :: Transform3D tag space -> VectorCurve3D units space -> VectorCurve3D units space
