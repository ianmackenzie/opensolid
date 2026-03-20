module OpenSolid.VectorCurve
  ( VectorCurve
  , Exists
  , Nondegenerate
  , isZero
  , singular0
  , singular1
  , constant
  , zero
  , bezier
  , value
  , bounds
  , derivative
  , secondDerivative
  , derivativeValue
  , derivativeBounds
  , secondDerivativeValue
  , secondDerivativeBounds
  , squaredMagnitude_
  , squaredMagnitude
  , nondegenerate
  , magnitude
  , direction
  , directionBounds
  , zeros
  , desingularized
  , desingularize
  , desingularizedQuotient
  , erase
  , unerase
  , coerce
  )
where

import Data.Void (Void)
import OpenSolid.Bezier qualified as Bezier
import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import {-# SOURCE #-} OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.Zero qualified
import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.DirectionBounds (DirectionBounds)
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.DirectionCurve (DirectionCurve)
import {-# SOURCE #-} OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.Nondegenerate (IsDegenerate (IsDegenerate), Nondegenerate (Nondegenerate))
import OpenSolid.Nondegenerate qualified as Nondegenerate
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Result qualified as Result
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector
import OpenSolid.VectorBounds (VectorBounds)
import OpenSolid.VectorBounds qualified as VectorBounds
import {-# SOURCE #-} OpenSolid.VectorCurve.Nondegenerate qualified as VectorCurve.Nondegenerate
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)
import {-# SOURCE #-} OpenSolid.VectorCurve2D qualified as VectorCurve2D
import {-# SOURCE #-} OpenSolid.VectorCurve3D (VectorCurve3D)
import {-# SOURCE #-} OpenSolid.VectorCurve3D qualified as VectorCurve3D

type family
  VectorCurve dimension units space =
    vectorCurve | vectorCurve -> dimension units space
  where
  VectorCurve 1 units Void = Curve1D units
  VectorCurve 2 units space = VectorCurve2D units space
  VectorCurve 3 units space = VectorCurve3D units space

class
  ( Vector.Exists dimension units space
  , VectorBounds.Exists dimension units space
  , Exists dimension Unitless space
  , HasUnits (VectorCurve dimension units space) units
  , Units.Coercion (VectorCurve dimension units space) (VectorCurve dimension Unitless space)
  , Units.Coercion (VectorCurve dimension Unitless space) (VectorCurve dimension units space)
  , Negation (VectorCurve dimension units space)
  , Addition
      (VectorCurve dimension units space)
      (VectorCurve dimension units space)
      (VectorCurve dimension units space)
  , Subtraction
      (VectorCurve dimension units space)
      (VectorCurve dimension units space)
      (VectorCurve dimension units space)
  , Multiplication Number (VectorCurve dimension units space) (VectorCurve dimension units space)
  , Multiplication (VectorCurve dimension units space) Number (VectorCurve dimension units space)
  , Multiplication
      (Curve1D Unitless)
      (VectorCurve dimension units space)
      (VectorCurve dimension units space)
  , Multiplication
      (VectorCurve dimension units space)
      (Curve1D Unitless)
      (VectorCurve dimension units space)
  , Division
      (VectorCurve dimension units space)
      (Nonzero (Curve1D Unitless))
      (VectorCurve dimension units space)
  , Division
      (VectorCurve dimension units space)
      (Nonzero (Curve1D units))
      (VectorCurve dimension Unitless space)
  , Division
      (VectorCurve dimension units space)
      (Nondegenerate (Curve1D Unitless))
      (VectorCurve dimension units space)
  , Division
      (VectorCurve dimension units space)
      (Nondegenerate (Curve1D units))
      (VectorCurve dimension Unitless space)
  , DotMultiplication (VectorCurve dimension Unitless space) (VectorCurve dimension units space) (Curve1D units)
  , DotMultiplication (VectorCurve dimension units space) (VectorCurve dimension Unitless space) (Curve1D units)
  , DotMultiplication_ (VectorCurve dimension units space) (VectorCurve dimension units space) (Curve1D (units ?*? units))
  , NewtonRaphson.Curve dimension units space
  ) =>
  Exists dimension units space
  where
  constant :: Vector dimension units space -> VectorCurve dimension units space
  isZero :: Tolerance units => VectorCurve dimension units space -> Bool
  singular0 :: VectorCurve dimension units space -> Bool
  singular1 :: VectorCurve dimension units space -> Bool
  bezier :: NonEmpty (Vector dimension units space) -> VectorCurve dimension units space
  value :: VectorCurve dimension units space -> Number -> Vector dimension units space
  bounds :: VectorCurve dimension units space -> Interval Unitless -> VectorBounds dimension units space
  derivative :: VectorCurve dimension units space -> VectorCurve dimension units space
  squaredMagnitude_ :: VectorCurve dimension units space -> Curve1D (units ?*? units)
  desingularized ::
    VectorCurve dimension units space ->
    VectorCurve dimension units space ->
    VectorCurve dimension units space ->
    VectorCurve dimension units space

instance Exists 1 units Void where
  constant = Curve1D.constant
  isZero curve = curve ~= Curve1D.constant Quantity.zero
  singular0 curve =
    Tolerance.using (Curve1D.singularityTolerance curve) do
      Curve1D.value curve 0.0 ~= Quantity.zero
  singular1 curve =
    Tolerance.using (Curve1D.singularityTolerance curve) do
      Curve1D.value curve 1.0 ~= Quantity.zero
  derivative = Curve1D.derivative
  value = Curve1D.value
  bounds = Curve1D.bounds
  bezier = Curve1D.bezier
  desingularized = Curve1D.desingularized
  squaredMagnitude_ = Curve1D.squared_

instance Exists 2 units space where
  constant = VectorCurve2D.constant
  isZero = VectorCurve2D.isZero
  singular0 = VectorCurve2D.singular0
  singular1 = VectorCurve2D.singular1
  derivative = VectorCurve2D.derivative
  value = VectorCurve2D.value
  bounds = VectorCurve2D.bounds
  bezier = VectorCurve2D.bezier
  desingularized = VectorCurve2D.desingularized
  squaredMagnitude_ = VectorCurve2D.squaredMagnitude_

instance Exists 3 units space where
  constant = VectorCurve3D.constant
  isZero = VectorCurve3D.isZero
  singular0 = VectorCurve3D.singular0
  singular1 = VectorCurve3D.singular1
  derivative = VectorCurve3D.derivative
  value = VectorCurve3D.value
  bounds = VectorCurve3D.bounds
  bezier = VectorCurve3D.bezier
  desingularized = VectorCurve3D.desingularized
  squaredMagnitude_ = VectorCurve3D.squaredMagnitude_

zero :: Exists dimension units space => VectorCurve dimension units space
zero = constant Vector.zero

nondegenerate ::
  (Exists dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  Result IsDegenerate (Nondegenerate (VectorCurve dimension units space))
nondegenerate curve = if isZero curve then Error IsDegenerate else Ok (Nondegenerate curve)

secondDerivative ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  VectorCurve dimension units space
secondDerivative = derivative . derivative

derivativeValue ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Number ->
  Vector dimension units space
derivativeValue curve tValue = value (derivative curve) tValue

derivativeBounds ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
derivativeBounds curve tBounds = bounds (derivative curve) tBounds

secondDerivativeValue ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Number ->
  Vector dimension units space
secondDerivativeValue curve tValue = value (secondDerivative curve) tValue

secondDerivativeBounds ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
secondDerivativeBounds curve tBounds = bounds (secondDerivative curve) tBounds

direction ::
  (Exists dimension units space, DirectionCurve.Exists dimension space, Tolerance units) =>
  VectorCurve dimension units space ->
  Result IsDegenerate (DirectionCurve dimension space)
direction vectorCurve = Result.map VectorCurve.Nondegenerate.direction (nondegenerate vectorCurve)

directionBounds ::
  (Exists dimension units space, DirectionBounds.Exists dimension space) =>
  VectorCurve dimension units space ->
  Interval Unitless ->
  DirectionBounds dimension space
directionBounds curve tBounds =
  DirectionBounds.unsafe $
    VectorBounds.normalize $
      if
        | Interval.lower tBounds == 0.0 && singular0 curve ->
            derivativeBounds curve tBounds
        | Interval.upper tBounds == 1.0 && singular1 curve ->
            negate (derivativeBounds curve tBounds)
        | otherwise -> bounds curve tBounds

zeros ::
  (Exists dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  Result IsDegenerate (List Number)
zeros vectorCurve =
  case Tolerance.using (Quantity.squared_ ?tolerance) (Curve1D.zeros (squaredMagnitude_ vectorCurve)) of
    Ok zeros1D -> Ok (List.map (.location) zeros1D)
    Error Curve1D.IsZero -> Error IsDegenerate

desingularize ::
  Exists dimension units space =>
  Maybe (Vector dimension units space, Vector dimension units space) ->
  VectorCurve dimension units space ->
  Maybe (Vector dimension units space, Vector dimension units space) ->
  VectorCurve dimension units space
desingularize Nothing curve Nothing = curve
desingularize startSingularity curve endSingularity = do
  let startCurve = case startSingularity of
        Nothing -> curve
        Just (value0, firstDerivative0) -> do
          let t0 = Desingularization.t0
          let valueT0 = value curve t0
          let firstDerivativeT0 = derivativeValue curve t0
          let secondDerivativeT0 = secondDerivativeValue curve t0
          bezier $
            Bezier.syntheticStart
              value0
              firstDerivative0
              valueT0
              firstDerivativeT0
              secondDerivativeT0
  let endCurve = case endSingularity of
        Nothing -> curve
        Just (value1, firstDerivative1) -> do
          let t1 = Desingularization.t1
          let valueT1 = value curve t1
          let firstDerivativeT1 = derivativeValue curve t1
          let secondDerivativeT1 = secondDerivativeValue curve t1
          bezier $
            Bezier.syntheticEnd
              valueT1
              firstDerivativeT1
              secondDerivativeT1
              value1
              firstDerivative1
  desingularized startCurve curve endCurve

desingularizedQuotient ::
  ( Exists dimension units1 space
  , Exists dimension (units1 ?/? units2) space
  ) =>
  VectorCurve dimension units1 space ->
  Nondegenerate (Curve1D units2) ->
  VectorCurve dimension (units1 ?/? units2) space
desingularizedQuotient lhs (Nondegenerate rhs) = do
  let singularityTolerance = Curve1D.singularityTolerance rhs
  let maybeSingularity tValue =
        if Tolerance.using singularityTolerance (Curve1D.value rhs tValue ~= Quantity.zero)
          then Just (lHopital lhs rhs tValue)
          else Nothing
  let interiorQuotient = unerase (erase lhs / Nonzero (Curve1D.erase rhs))
  desingularize (maybeSingularity 0.0) interiorQuotient (maybeSingularity 1.0)

lHopital ::
  ( Exists dimension units1 space
  , Exists dimension (units1 ?/? units2) space
  ) =>
  VectorCurve dimension units1 space ->
  Curve1D units2 ->
  Number ->
  ( Vector dimension (units1 ?/? units2) space
  , Vector dimension (units1 ?/? units2) space
  )
lHopital lhs rhs tValue = do
  let lhs' = Vector.erase (derivativeValue lhs tValue)
  let lhs'' = Vector.erase (secondDerivativeValue lhs tValue)
  let rhs' = Vector.erase (derivativeValue rhs tValue)
  let rhs'' = Vector.erase (secondDerivativeValue rhs tValue)
  let value_ = lhs' / rhs'
  let firstDerivative_ = (lhs'' * rhs' - lhs' * rhs'') / (2.0 * Quantity.squared rhs')
  (Vector.unerase value_, Vector.unerase firstDerivative_)

squaredMagnitude ::
  (Exists dimension units1 space, Units.Squared units1 units2) =>
  VectorCurve dimension units1 space ->
  Curve1D units2
squaredMagnitude curve = Units.specialize (squaredMagnitude_ curve)

magnitude ::
  (Exists dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  Curve1D units
magnitude curve =
  if isZero curve
    then Curve1D.zero
    else Nondegenerate.unwrap (VectorCurve.Nondegenerate.magnitude (Nondegenerate curve))

erase ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  VectorCurve dimension Unitless space
erase = Units.erase

unerase ::
  Exists dimension units space =>
  VectorCurve dimension Unitless space ->
  VectorCurve dimension units space
unerase = Units.unerase

coerce ::
  (Exists dimension units1 space, Exists dimension units2 space) =>
  VectorCurve dimension units1 space ->
  VectorCurve dimension units2 space
coerce curve = unerase (erase curve)
