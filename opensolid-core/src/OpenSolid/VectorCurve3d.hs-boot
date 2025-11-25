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
data VectorCurve3d space units

instance HasField "compiled" (VectorCurve3d space units) (Compiled space units)

instance HasField "derivative" (VectorCurve3d space units) (VectorCurve3d space units)

type Compiled space units =
  CompiledFunction
    Number
    (Vector3d space units)
    (Bounds Unitless)
    (VectorBounds3d space units)

instance HasUnits (VectorCurve3d space units) units

instance Negation (VectorCurve3d space units)

instance Multiplication Sign (VectorCurve3d space units) (VectorCurve3d space units)

instance Multiplication (VectorCurve3d space units) Sign (VectorCurve3d space units)

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve3d space1 unitsA) (VectorCurve3d space2 unitsB)

instance
  Multiplication_
    (Curve units1)
    (VectorCurve3d space units2)
    (VectorCurve3d space (units1 ?*? units2))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (VectorCurve3d space units2) (VectorCurve3d space units3)

instance
  Multiplication_
    (VectorCurve3d space units1)
    (Curve units2)
    (VectorCurve3d space (units1 ?*? units2))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3d space units1) (Curve units2) (VectorCurve3d space units3)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve3d space1 units1)
    (VectorCurve3d space2 units2)
    (Curve (units1 ?*? units2))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve3d space1 units1)
    (VectorCurve3d space2 units2)
    (Curve units3)

constant :: Vector3d space units -> VectorCurve3d space units
new :: Compiled space units -> VectorCurve3d space units -> VectorCurve3d space units
on :: Plane3d space (Defines local) -> VectorCurve2d local units -> VectorCurve3d space units
evaluate :: VectorCurve3d space units -> Number -> Vector3d space units
evaluateBounds :: VectorCurve3d space units -> Bounds Unitless -> VectorBounds3d space units
quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorCurve3d space units1 ->
  Curve units2 ->
  Result DivisionByZero (VectorCurve3d space units3)
quotient_ ::
  Tolerance units2 =>
  VectorCurve3d space units1 ->
  Curve units2 ->
  Result DivisionByZero (VectorCurve3d space (units1 ?/? units2))
unsafeQuotient ::
  Units.Quotient units1 units2 units3 =>
  VectorCurve3d space units1 ->
  Curve units2 ->
  VectorCurve3d space units3
unsafeQuotient_ ::
  VectorCurve3d space units1 ->
  Curve units2 ->
  VectorCurve3d space (units1 ?/? units2)
magnitude :: Tolerance units => VectorCurve3d space units -> Curve units
transformBy :: Transform3d tag space -> VectorCurve3d space units -> VectorCurve3d space units
