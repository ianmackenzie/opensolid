module OpenSolid.VectorCurve2D
  ( VectorCurve2D
  , Compiled
  , constant
  , new
  , compiled
  , derivative
  , isZero
  , evaluate
  , evaluateBounds
  , normalize
  , quotient
  , quotient_
  , transformBy
  )
where

import {-# SOURCE #-} OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import OpenSolid.DivisionByZero (DivisionByZero)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Primitives (Transform2D, Vector2D, VectorBounds2D)
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units

type role VectorCurve2D nominal nominal

type VectorCurve2D :: Type -> Type -> Type
data VectorCurve2D units space

type Compiled units space =
  CompiledFunction
    Number
    (Vector2D units space)
    (Interval Unitless)
    (VectorBounds2D units space)

instance HasUnits (VectorCurve2D units space) units

instance Negation (VectorCurve2D units space)

instance Multiplication Sign (VectorCurve2D units space) (VectorCurve2D units space)

instance Multiplication (VectorCurve2D units space) Sign (VectorCurve2D units space)

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve2D unitsA space1) (VectorCurve2D unitsB space2)

instance
  Multiplication_
    (Curve1D units1)
    (VectorCurve2D units2 space)
    (VectorCurve2D (units1 ?*? units2) space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1D units1) (VectorCurve2D units2 space) (VectorCurve2D units3 space)

instance
  Multiplication_
    (VectorCurve2D units1 space)
    (Curve1D units2)
    (VectorCurve2D (units1 ?*? units2) space)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve2D units1 space) (Curve1D units2) (VectorCurve2D units3 space)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve2D units1 space1)
    (VectorCurve2D units2 space2)
    (Curve1D (units1 ?*? units2))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve2D units1 space1)
    (VectorCurve2D units2 space2)
    (Curve1D units3)

constant :: Vector2D units space -> VectorCurve2D units space
new :: Compiled units space -> VectorCurve2D units space -> VectorCurve2D units space
compiled :: VectorCurve2D units space -> Compiled units space
derivative :: VectorCurve2D units space -> VectorCurve2D units space
isZero :: Tolerance units => VectorCurve2D units space -> Bool
evaluate :: VectorCurve2D units space -> Number -> Vector2D units space
evaluateBounds :: VectorCurve2D units space -> Interval Unitless -> VectorBounds2D units space
quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorCurve2D units1 space ->
  Curve1D units2 ->
  Result DivisionByZero (VectorCurve2D units3 space)
quotient_ ::
  Tolerance units2 =>
  VectorCurve2D units1 space ->
  Curve1D units2 ->
  Result DivisionByZero (VectorCurve2D (units1 ?/? units2) space)
normalize :: Tolerance units => VectorCurve2D units space -> VectorCurve2D Unitless space
transformBy ::
  Transform2D tag translationUnits space ->
  VectorCurve2D units space ->
  VectorCurve2D units space
