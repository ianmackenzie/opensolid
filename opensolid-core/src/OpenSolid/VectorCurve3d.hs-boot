module OpenSolid.VectorCurve3d
  ( VectorCurve3d
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
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Prelude
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.VectorBounds3d (VectorBounds3d)
import {-# SOURCE #-} OpenSolid.VectorCurve2d (VectorCurve2d)

type role VectorCurve3d nominal nominal

type VectorCurve3d :: Type -> Type -> Type
data VectorCurve3d units space

instance HasField "compiled" (VectorCurve3d units space) (Compiled units space)

instance HasField "derivative" (VectorCurve3d units space) (VectorCurve3d units space)

type Compiled units space =
  CompiledFunction
    Number
    (Vector3d units space)
    (Bounds Unitless)
    (VectorBounds3d units space)

instance HasUnits (VectorCurve3d units space) units

instance Negation (VectorCurve3d units space)

instance Multiplication Sign (VectorCurve3d units space) (VectorCurve3d units space)

instance Multiplication (VectorCurve3d units space) Sign (VectorCurve3d units space)

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve3d unitsA space1) (VectorCurve3d unitsB space2)

instance
  Multiplication_
    (Curve units1)
    (VectorCurve3d units2 space)
    (VectorCurve3d (units1 ?*? units2) space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (VectorCurve3d units2 space) (VectorCurve3d units3 space)

instance
  Multiplication_
    (VectorCurve3d units1 space)
    (Curve units2)
    (VectorCurve3d (units1 ?*? units2) space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3d units1 space) (Curve units2) (VectorCurve3d units3 space)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve3d units1 space1)
    (VectorCurve3d units2 space2)
    (Curve (units1 ?*? units2))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve3d units1 space1)
    (VectorCurve3d units2 space2)
    (Curve units3)

constant :: Vector3d units space -> VectorCurve3d units space
new :: Compiled units space -> VectorCurve3d units space -> VectorCurve3d units space
on :: Plane3d global local -> VectorCurve2d units local -> VectorCurve3d units global
evaluate :: VectorCurve3d units space -> Number -> Vector3d units space
evaluateBounds :: VectorCurve3d units space -> Bounds Unitless -> VectorBounds3d units space
quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorCurve3d units1 space ->
  Curve units2 ->
  Result DivisionByZero (VectorCurve3d units3 space)
quotient_ ::
  Tolerance units2 =>
  VectorCurve3d units1 space ->
  Curve units2 ->
  Result DivisionByZero (VectorCurve3d (units1 ?/? units2) space)
unsafeQuotient ::
  Units.Quotient units1 units2 units3 =>
  VectorCurve3d units1 space ->
  Curve units2 ->
  VectorCurve3d units3 space
unsafeQuotient_ ::
  VectorCurve3d units1 space ->
  Curve units2 ->
  VectorCurve3d (units1 ?/? units2) space
magnitude :: Tolerance units => VectorCurve3d units space -> Curve units
transformBy :: Transform3d tag space -> VectorCurve3d units space -> VectorCurve3d units space
