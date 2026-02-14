module OpenSolid.VectorCurve
  ( VectorCurve
  , Exists
  , IsZero (IsZero)
  , isZero
  , constant
  , zero
  , bezier
  , evaluate
  , evaluateBounds
  , derivative
  , squaredMagnitude_
  , squaredMagnitude
  , magnitude
  , normalize
  , unsafeNormalize
  , direction
  , zeros
  , desingularized
  , desingularize
  , desingularizedQuotient
  , erase
  , unerase
  )
where

import Data.Void (Void)
import OpenSolid.Bezier qualified as Bezier
import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import {-# SOURCE #-} OpenSolid.Curve1D qualified as Curve1D
import {-# SOURCE #-} OpenSolid.Curve1D qualified as Curve1d
import OpenSolid.Curve1D.Zero qualified
import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.DirectionCurve (DirectionCurve)
import {-# SOURCE #-} OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Interval (Interval)
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector
import OpenSolid.VectorBounds (VectorBounds)
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

data IsZero = IsZero deriving (Eq, Show)

class
  ( Vector.Exists dimension units space
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
      (Curve1D.WithNoZeros Unitless)
      (VectorCurve dimension units space)
  , Division
      (VectorCurve dimension units space)
      (Curve1D.WithNoZeros units)
      (VectorCurve dimension Unitless space)
  , Division
      (VectorCurve dimension units space)
      (Curve1D.WithNoInteriorZeros Unitless)
      (VectorCurve dimension units space)
  , Division
      (VectorCurve dimension units space)
      (Curve1D.WithNoInteriorZeros units)
      (VectorCurve dimension Unitless space)
  , NewtonRaphson.Curve dimension units space
  ) =>
  Exists dimension units space
  where
  constant :: Vector dimension units space -> VectorCurve dimension units space
  isZero :: Tolerance units => VectorCurve dimension units space -> Bool
  bezier :: NonEmpty (Vector dimension units space) -> VectorCurve dimension units space
  evaluate :: VectorCurve dimension units space -> Number -> Vector dimension units space
  evaluateBounds :: VectorCurve dimension units space -> Interval Unitless -> VectorBounds dimension units space
  derivative :: VectorCurve dimension units space -> VectorCurve dimension units space
  squaredMagnitude_ :: VectorCurve dimension units space -> Curve1D (units ?*? units)
  unsafeNormalize :: VectorCurve dimension units space -> VectorCurve dimension Unitless space
  desingularized ::
    VectorCurve dimension units space ->
    VectorCurve dimension units space ->
    VectorCurve dimension units space ->
    VectorCurve dimension units space

instance Exists 1 units Void where
  constant = Curve1D.constant
  isZero curve = curve ~= Curve1D.constant Quantity.zero
  derivative = Curve1D.derivative
  evaluate = Curve1D.evaluate
  evaluateBounds = Curve1D.evaluateBounds
  bezier = Curve1D.bezier
  desingularized = Curve1D.desingularized
  squaredMagnitude_ = Curve1D.squared_
  unsafeNormalize curve =
    -- If a 1D curve has no interior zeros,
    -- then it is either always non-negative or always non-positive,
    -- and so the normalized version of that curve (the curve divided by its magnitude)
    -- will be a constant equal to either positive or negative one
    if evaluate curve 0.5 > Quantity.zero then constant 1.0 else constant -1.0

instance Exists 2 units space where
  constant = VectorCurve2D.constant
  isZero = VectorCurve2D.isZero
  derivative = VectorCurve2D.derivative
  evaluate = VectorCurve2D.evaluate
  evaluateBounds = VectorCurve2D.evaluateBounds
  bezier = VectorCurve2D.bezier
  desingularized = VectorCurve2D.desingularized
  unsafeNormalize = VectorCurve2D.unsafeNormalize
  squaredMagnitude_ = VectorCurve2D.squaredMagnitude_

instance Exists 3 units space where
  constant = VectorCurve3D.constant
  isZero = VectorCurve3D.isZero
  derivative = VectorCurve3D.derivative
  evaluate = VectorCurve3D.evaluate
  evaluateBounds = VectorCurve3D.evaluateBounds
  bezier = VectorCurve3D.bezier
  desingularized = VectorCurve3D.desingularized
  unsafeNormalize = VectorCurve3D.unsafeNormalize
  squaredMagnitude_ = VectorCurve3D.squaredMagnitude_

zero :: Exists dimension units space => VectorCurve dimension units space
zero = constant Vector.zero

normalize ::
  (Exists dimension units space, DirectionCurve.Exists dimension space, Tolerance units) =>
  VectorCurve dimension units space ->
  VectorCurve dimension Unitless space
normalize curve = if isZero curve then zero else unsafeNormalize curve

direction ::
  (Exists dimension units space, DirectionCurve.Exists dimension space, Tolerance units) =>
  VectorCurve dimension units space ->
  Result IsZero (DirectionCurve dimension space)
direction vectorCurve =
  if isZero vectorCurve
    then Error IsZero
    else Ok (DirectionCurve.unsafe (unsafeNormalize vectorCurve))

zeros ::
  (Exists dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  Result IsZero (List Number)
zeros vectorCurve =
  case Tolerance.using (Quantity.squared_ ?tolerance) (Curve1D.zeros (squaredMagnitude_ vectorCurve)) of
    Ok zeros1D -> Ok (List.map (.location) zeros1D)
    Error Curve1D.IsZero -> Error IsZero

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
          let valueT0 = evaluate curve t0
          let firstDerivativeT0 = evaluate (derivative curve) t0
          let secondDerivativeT0 = evaluate (derivative (derivative curve)) t0
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
          let valueT1 = evaluate curve t1
          let firstDerivativeT1 = evaluate (derivative curve) t1
          let secondDerivativeT1 = evaluate (derivative (derivative curve)) t1
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
  Curve1d.WithNoInteriorZeros units2 ->
  VectorCurve dimension (units1 ?/? units2) space
desingularizedQuotient lhs (Curve1D.WithNoInteriorZeros rhs) = do
  let singularityTolerance = Curve1D.singularityTolerance rhs
  let maybeSingularity tValue =
        if Tolerance.using singularityTolerance (Curve1D.evaluate rhs tValue ~= Quantity.zero)
          then Just (lHopital lhs rhs tValue)
          else Nothing
  let interiorQuotient = unerase (erase lhs / Curve1D.WithNoZeros (Curve1D.erase rhs))
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
  let lhs' = Vector.erase (evaluate (derivative lhs) tValue)
  let lhs'' = Vector.erase (evaluate (derivative (derivative lhs)) tValue)
  let rhs' = Vector.erase (evaluate (derivative rhs) tValue)
  let rhs'' = Vector.erase (evaluate (derivative (derivative rhs)) tValue)
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
magnitude curve = Curve1D.sqrt_ (squaredMagnitude_ curve)

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
