{-# OPTIONS_GHC -Wno-orphans #-}

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
  , singular0
  , singular1
  , value
  , bounds
  , quotient
  , quotient_
  , squaredMagnitude_
  , magnitude
  , transformBy
  )
where

import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import OpenSolid.DivisionByZero (DivisionByZero)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Primitives (Plane3D, Transform3D, Vector3D, VectorBounds3D)
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)

type VectorCurve3D units space = VectorCurve 3 units space

type Compiled units space = VectorCurve.Compiled 3 units space

constant :: Vector3D units space -> VectorCurve3D units space
new :: Compiled units space -> VectorCurve3D units space -> VectorCurve3D units space
on :: Plane3D global local -> VectorCurve2D units local -> VectorCurve3D units global
bezier :: NonEmpty (Vector3D units space) -> VectorCurve3D units space
compiled :: VectorCurve3D units space -> Compiled units space
derivative :: VectorCurve3D units space -> VectorCurve3D units space
isZero :: Tolerance units => VectorCurve3D units space -> Bool
singular0 :: VectorCurve3D units space -> Bool
singular1 :: VectorCurve3D units space -> Bool
value :: VectorCurve3D units space -> Number -> Vector3D units space
bounds :: VectorCurve3D units space -> Interval Unitless -> VectorBounds3D units space
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
magnitude :: Tolerance units => VectorCurve3D units space -> Curve1D units
transformBy :: Transform3D tag space -> VectorCurve3D units space -> VectorCurve3D units space
