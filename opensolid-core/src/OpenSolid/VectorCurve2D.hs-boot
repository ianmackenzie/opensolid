module OpenSolid.VectorCurve2D
  ( VectorCurve2D
  , Compiled
  , Nondegenerate
  , constant
  , new
  , bezier
  , compiled
  , derivative
  , isZero
  , singular0
  , singular1
  , value
  , bounds
  , squaredMagnitude_
  , quotient
  , quotient_
  , transformBy
  )
where

import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import OpenSolid.DivisionByZero (DivisionByZero)
import OpenSolid.Interval (Interval)
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Prelude
import OpenSolid.Primitives (Transform2D, Vector2D, VectorBounds2D)
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve

type VectorCurve2D units space = VectorCurve 2 units space

type Compiled units space = VectorCurve.Compiled 2 units space

constant :: Vector2D units space -> VectorCurve2D units space
new :: Compiled units space -> VectorCurve2D units space -> VectorCurve2D units space
bezier :: NonEmpty (Vector2D units space) -> VectorCurve2D units space
compiled :: VectorCurve2D units space -> Compiled units space
derivative :: VectorCurve2D units space -> VectorCurve2D units space
isZero :: Tolerance units => VectorCurve2D units space -> Bool
singular0 :: VectorCurve2D units space -> Bool
singular1 :: VectorCurve2D units space -> Bool
value :: VectorCurve2D units space -> Number -> Vector2D units space
bounds :: VectorCurve2D units space -> Interval Unitless -> VectorBounds2D units space
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
squaredMagnitude_ :: VectorCurve2D units space -> Curve1D (units ?*? units)
transformBy ::
  Transform2D tag translationUnits space ->
  VectorCurve2D units space ->
  VectorCurve2D units space
