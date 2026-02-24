module Tests.Estimate (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Area qualified as Area
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Estimate (Estimate)
import OpenSolid.Estimate qualified as Estimate
import OpenSolid.Int qualified as Int
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Pair qualified as Pair
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.VectorCurve2D qualified as VectorCurve2D
import Test (Test)
import Test qualified
import Tests.Random qualified as Random

tests :: Tolerance Meters => List Test
tests =
  [ area
  , minimumBy
  , maximumBy
  , pickMinimumBy
  , pickMaximumBy
  ]

data DummyEstimate = DummyEstimate Length (Interval Meters)

instance Estimate.Interface DummyEstimate Meters where
  boundsImpl (DummyEstimate _ bounds) = bounds
  refineImpl (DummyEstimate value (Interval low high)) = do
    let refinedBounds = Interval (Quantity.midpoint low value) (Quantity.midpoint value high)
    Estimate.new (DummyEstimate value refinedBounds)

dummyEstimate :: Generator (Length, Estimate Meters)
dummyEstimate = do
  bounds <- Interval.random Random.length
  t <- Parameter.random
  let value = Interval.interpolate bounds t
  Random.return (value, Estimate.new (DummyEstimate value bounds))

duplicatedDummyEstimates :: Generator (NonEmpty (Length, Estimate Meters))
duplicatedDummyEstimates = do
  pair <- dummyEstimate
  numPairs <- Int.random 1 3
  Random.return (pair :| List.replicate (numPairs - 1) pair)

dummyEstimates :: Generator (NonEmpty (Length, Estimate Meters))
dummyEstimates = do
  nested <- NonEmpty.random 10 duplicatedDummyEstimates
  let flattened = NonEmpty.concat nested
  NonEmpty.shuffle flattened

resolvesTo :: Tolerance units => Quantity units -> Estimate units -> Result Text Bool
resolvesTo value estimate
  | not (value `intersects` Estimate.bounds estimate) = Ok False
  | Interval.width (Estimate.bounds estimate) ~= Quantity.zero = Ok True
  | otherwise = do
      let refinedEstimate = Estimate.refine estimate
      if Interval.width (Estimate.bounds refinedEstimate) < Interval.width (Estimate.bounds estimate)
        then resolvesTo value refinedEstimate
        else Error "Refined bounds are not narrower than original bounds"

area :: Tolerance Meters => Test
area = Test.verify "area" Test.do
  let curve =
        Curve2D.polarArc
          (#centerPoint Point2D.origin)
          (#radius Length.meter)
          (#startAngle Angle.pi)
          (#endAngle Angle.zero)
  let dAdt = Curve2D.yCoordinate curve * VectorCurve2D.xComponent (Curve2D.derivative curve)
  let areaEstimate = Curve1D.integrate dAdt
  let expectedArea = Area.squareMeters Number.halfPi
  areaIsCorrect <- Tolerance.using (Area.squareMeters 1e-4) (resolvesTo expectedArea areaEstimate)
  Test.expect areaIsCorrect

minimumBy :: Tolerance Meters => Test
minimumBy = Test.check 100 "minimumBy" Test.do
  valuesAndEstimates <- dummyEstimates
  let minimumValue = NonEmpty.minimumOf Pair.first valuesAndEstimates
  let minimumValueFromEstimates = Pair.first (Estimate.minimumBy Pair.second valuesAndEstimates)
  Test.expect (minimumValueFromEstimates == minimumValue)

maximumBy :: Tolerance Meters => Test
maximumBy = Test.check 100 "maximumBy" Test.do
  valuesAndEstimates <- dummyEstimates
  let maximumValue = NonEmpty.maximumOf Pair.first valuesAndEstimates
  let maximumValueFromEstimates = Pair.first (Estimate.maximumBy Pair.second valuesAndEstimates)
  Test.expect (maximumValueFromEstimates == maximumValue)

pickMinimumBy :: Tolerance Meters => Test
pickMinimumBy = Test.check 100 "pickMinimumBy" Test.do
  valuesAndEstimates <- dummyEstimates
  let (minPair, remainingPairs) = Estimate.pickMinimumBy Pair.second valuesAndEstimates
  let minValue = Pair.first minPair
  let remainingValues = List.map Pair.first remainingPairs
  let originalValues = NonEmpty.map Pair.first valuesAndEstimates
  let minValueIsCorrect = List.allSatisfy (>= minValue) remainingValues
  let allValuesArePresent =
        NonEmpty.sort originalValues == NonEmpty.sort (minValue :| remainingValues)
  Test.expect (minValueIsCorrect && allValuesArePresent)

pickMaximumBy :: Tolerance Meters => Test
pickMaximumBy = Test.check 100 "pickMaximumBy" Test.do
  valuesAndEstimates <- dummyEstimates
  let (maxPair, remainingPairs) = Estimate.pickMaximumBy Pair.second valuesAndEstimates
  let maxValue = Pair.first maxPair
  let remainingValues = List.map Pair.first remainingPairs
  let originalValues = NonEmpty.map Pair.first valuesAndEstimates
  let maxValueIsCorrect = List.allSatisfy (<= maxValue) remainingValues
  let allValuesArePresent =
        NonEmpty.sort originalValues == NonEmpty.sort (maxValue :| remainingValues)
  Test.expect (maxValueIsCorrect && allValuesArePresent)
