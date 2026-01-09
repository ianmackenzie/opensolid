module OpenSolid.VectorCurve2d
  ( VectorCurve2d
  , Compiled
  , constant
  , new
  , compiled
  , derivative
  , evaluate
  , evaluateBounds
  , quotient
  , quotient_
  , transformBy
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve (Curve)
import OpenSolid.DivisionByZero (DivisionByZero)
import OpenSolid.Polymorphic.Vector2d (Vector2d)
import OpenSolid.Prelude
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.VectorBounds2d (VectorBounds2d)

type role VectorCurve2d nominal nominal

type VectorCurve2d :: Type -> Type -> Type
data VectorCurve2d units space

type Compiled units space =
  CompiledFunction
    Number
    (Vector2d units space)
    (Bounds Unitless)
    (VectorBounds2d units space)

instance HasUnits (VectorCurve2d units space) units

instance Negation (VectorCurve2d units space)

instance Multiplication Sign (VectorCurve2d units space) (VectorCurve2d units space)

instance Multiplication (VectorCurve2d units space) Sign (VectorCurve2d units space)

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve2d unitsA space1) (VectorCurve2d unitsB space2)

instance
  Multiplication_
    (Curve units1)
    (VectorCurve2d units2 space)
    (VectorCurve2d (units1 ?*? units2) space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (VectorCurve2d units2 space) (VectorCurve2d units3 space)

instance
  Multiplication_
    (VectorCurve2d units1 space)
    (Curve units2)
    (VectorCurve2d (units1 ?*? units2) space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve2d units1 space) (Curve units2) (VectorCurve2d units3 space)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve2d units1 space1)
    (VectorCurve2d units2 space2)
    (Curve (units1 ?*? units2))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve2d units1 space1)
    (VectorCurve2d units2 space2)
    (Curve units3)

constant :: Vector2d units space -> VectorCurve2d units space
new :: Compiled units space -> VectorCurve2d units space -> VectorCurve2d units space
compiled :: VectorCurve2d units space -> Compiled units space
derivative :: VectorCurve2d units space -> VectorCurve2d units space
evaluate :: VectorCurve2d units space -> Number -> Vector2d units space
evaluateBounds :: VectorCurve2d units space -> Bounds Unitless -> VectorBounds2d units space
quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorCurve2d units1 space ->
  Curve units2 ->
  Result DivisionByZero (VectorCurve2d units3 space)
quotient_ ::
  Tolerance units2 =>
  VectorCurve2d units1 space ->
  Curve units2 ->
  Result DivisionByZero (VectorCurve2d (units1 ?/? units2) space)
transformBy ::
  Transform2d tag translationUnits space ->
  VectorCurve2d units space ->
  VectorCurve2d units space
