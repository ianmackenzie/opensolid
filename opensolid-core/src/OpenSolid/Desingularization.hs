module OpenSolid.Desingularization
  ( Curve
  , t0
  , t1
  , value
  , bounds
  , continuity
  , curve
  , syntheticStart
  , syntheticEnd
  , curveQuotient_
  )
where

import OpenSolid.Bezier qualified as Bezier
import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import {-# SOURCE #-} OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Desingularization.Curve (Curve)
import OpenSolid.Desingularization.Curve qualified as Desingularization.Curve
import OpenSolid.Interval (Interval)
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Tolerance qualified as Tolerance

t0 :: Number
-- Should be kept in sync with T0 in bytecode.cpp
t0 = 0.0625 -- 1/16

t1 :: Number
-- Should be kept in sync with T1 in bytecode.cpp
t1 = 0.9375 -- 15/16

value :: Number -> a -> a -> a -> a
value t start middle end
  | t <= t0 = start
  | t >= t1 = end
  | otherwise = middle

bounds :: Interval Unitless -> a -> a -> a -> a
bounds t start middle end
  | t.upper <= t0 = start
  | t.lower >= t1 = end
  | otherwise = middle

{-| The order of continuity to use when joining a synthetic curve to a base curve
in order to 'desingularize' the base curve.
-}
continuity :: Int
continuity = 2

syntheticStart :: Bezier.Constraints p v => p -> v -> p -> v -> v -> NonEmpty p
syntheticStart point0 firstDerivative0 pointT0 firstDerivativeT0 secondDerivativeT0 = do
  let segmentDerivatives0 = [t0 * firstDerivative0]
  let segmentDerivativesT0 = [t0 * firstDerivativeT0, t0 * t0 * secondDerivativeT0]
  Bezier.hermite point0 segmentDerivatives0 pointT0 segmentDerivativesT0
    & Bezier.segment 0.0 (1.0 / t0)

syntheticEnd :: Bezier.Constraints p v => p -> v -> v -> p -> v -> NonEmpty p
syntheticEnd pointT1 firstDerivativeT1 secondDerivativeT1 point1 firstDerivative1 = do
  let segmentDerivativesT1 = [t0 * firstDerivativeT1, t0 * t0 * secondDerivativeT1]
  let segmentDerivatives1 = [t0 * firstDerivative1]
  Bezier.hermite pointT1 segmentDerivativesT1 point1 segmentDerivatives1
    & Bezier.segment -(t1 / t0) 1.0

curve :: Curve c p v => Maybe (p, v) -> c -> Maybe (p, v) -> c
curve Nothing givenCurve Nothing = givenCurve
curve startSingularity givenCurve endSingularity = do
  let startCurve = case startSingularity of
        Nothing -> givenCurve
        Just (value0, firstDerivative0) ->
          Desingularization.Curve.bezier $
            syntheticStart
              value0
              firstDerivative0
              (Desingularization.Curve.value givenCurve t0)
              (Desingularization.Curve.derivativeValue givenCurve t0)
              (Desingularization.Curve.secondDerivativeValue givenCurve t0)
  let endCurve = case endSingularity of
        Nothing -> givenCurve
        Just (value1, firstDerivative1) ->
          Desingularization.Curve.bezier $
            syntheticEnd
              (Desingularization.Curve.value givenCurve t1)
              (Desingularization.Curve.derivativeValue givenCurve t1)
              (Desingularization.Curve.secondDerivativeValue givenCurve t1)
              value1
              firstDerivative1
  Desingularization.Curve.desingularized startCurve givenCurve endCurve

curveQuotient_ ::
  ( Curve c1 v1 v1
  , Division_ c1 (Nonzero (Curve1D units)) c2
  , Division_ v1 (Quantity units) v2
  , Curve c2 v2 v2
  ) =>
  c1 ->
  Nondegenerate (Curve1D units) ->
  c2
curveQuotient_ numerator (Nondegenerate denominator) = do
  let singularityTolerance = Curve1D.singularityTolerance denominator
  let maybeSingularity tValue =
        if Tolerance.using singularityTolerance (Curve1D.value denominator tValue ~= Quantity.zero)
          then Just (lHopital_ numerator denominator tValue)
          else Nothing
  let interiorQuotient = numerator ?/? Nonzero denominator
  curve (maybeSingularity 0.0) interiorQuotient (maybeSingularity 1.0)

lHopital_ ::
  (Curve c1 v1 v1, Division_ v1 (Quantity units) v2, Bezier.Vector v2) =>
  c1 ->
  Curve1D units ->
  Number ->
  (v2, v2)
lHopital_ numerator denominator tValue = do
  let numerator' = Desingularization.Curve.derivativeValue numerator tValue
  let numerator'' = Desingularization.Curve.secondDerivativeValue numerator tValue
  let denominator' = Curve1D.derivativeValue denominator tValue
  let denominator'' = Curve1D.secondDerivativeValue denominator tValue
  let value_ = numerator' ?/? denominator'
  let normalizedNumerator''_ = numerator'' ?/? denominator'
  let normalizedDenominator'' = denominator'' / denominator'
  let firstDerivative_ = 0.5 * (normalizedNumerator''_ - value_ * normalizedDenominator'')
  (value_, firstDerivative_)
