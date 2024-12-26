module Tests.Estimate (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Arc2d qualified as Arc2d
import OpenSolid.Area qualified as Area
import OpenSolid.Estimate (Estimate)
import OpenSolid.Estimate qualified as Estimate
import OpenSolid.Float qualified as Float
import OpenSolid.Int qualified as Int
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Curve1d qualified as Curve1d
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Qty qualified as Qty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import OpenSolid.Range (Range (Range))
import OpenSolid.Range qualified as Range
import Test (Test)
import Test qualified
import Tests.Random qualified as Random
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units (Meters)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

tests :: Tolerance Meters => List Test
tests =
  [ smallest
  , largest
  , area
  , minimumBy
  , maximumBy
  , smallestBy
  , largestBy
  , pickMinimumBy
  , pickMaximumBy
  , pickSmallestBy
  , pickLargestBy
  ]

data DummyEstimate = DummyEstimate Length (Range Meters)

instance Estimate.Interface DummyEstimate Meters where
  boundsImpl (DummyEstimate _ range) = range
  refineImpl (DummyEstimate value (Range low high)) = do
    let refinedRange = Range.from (Qty.midpoint low value) (Qty.midpoint value high)
    Estimate.new (DummyEstimate value refinedRange)

dummyEstimate :: Generator (Length, Estimate Meters)
dummyEstimate = Random.do
  range <- Range.random Random.length
  t <- Parameter.random
  let value = Range.interpolate range t
  Random.return (value, Estimate.new (DummyEstimate value range))

duplicatedDummyEstimates :: Generator (NonEmpty (Length, Estimate Meters))
duplicatedDummyEstimates = Random.do
  pair <- dummyEstimate
  numPairs <- Int.random 1 3
  Random.return (pair :| List.repeat (numPairs - 1) pair)

dummyEstimates :: Generator (NonEmpty (Length, Estimate Meters))
dummyEstimates = Random.do
  nested <- NonEmpty.random 10 duplicatedDummyEstimates
  let flattened = NonEmpty.concat nested
  NonEmpty.shuffle flattened

check :: Tolerance units => Estimate units -> Qty units -> (Bool, Range units)
check estimate value = do
  let bounds = Estimate.bounds estimate
  if
    | not (Range.includes value bounds) -> (False, bounds)
    | Range.width bounds ~= Qty.zero -> (True, bounds)
    | otherwise -> check (Estimate.refine estimate) value

smallest :: Tolerance Meters => Test
smallest = Test.check 100 "smallest" Test.do
  valuesAndEstimates <- dummyEstimates
  let (values, estimates) = NonEmpty.unzip2 valuesAndEstimates
  let bounds = NonEmpty.map Estimate.bounds estimates
  let smallestValue = Qty.smallest values
  let smallestEstimate = Estimate.smallest estimates
  let (valid, finalBounds) = check smallestEstimate smallestValue
  Test.expect valid
    |> Test.output "values" (Test.lines values)
    |> Test.output "smallestValue" smallestValue
    |> Test.output "initial bounds list" (Test.lines bounds)
    |> Test.output "finalBounds" finalBounds

largest :: Tolerance Meters => Test
largest = Test.check 100 "largest" Test.do
  valuesAndEstimates <- dummyEstimates
  let (values, estimates) = NonEmpty.unzip2 valuesAndEstimates
  let bounds = NonEmpty.map Estimate.bounds estimates
  let largestValue = Qty.largest values
  let largestEstimate = Estimate.largest estimates
  let (valid, finalBounds) = check largestEstimate largestValue
  Test.expect valid
    |> Test.output "values" (Test.lines values)
    |> Test.output "largestValue" largestValue
    |> Test.output "initial bounds list" (Test.lines bounds)
    |> Test.output "finalBounds" finalBounds

resolvesTo :: Tolerance units => Qty units -> Estimate units -> Result Text Bool
resolvesTo value estimate
  | not (value ^ Estimate.bounds estimate) = Success False
  | Estimate.bounds estimate ~= value = Success True
  | otherwise = do
      let refinedEstimate = Estimate.refine estimate
      if Range.width (Estimate.bounds refinedEstimate) < Range.width (Estimate.bounds estimate)
        then resolvesTo value refinedEstimate
        else Failure "Refined bounds are not narrower than original bounds"

area :: Tolerance Meters => Test
area = Test.verify "area" Test.do
  let curve = Arc2d.polar Point2d.origin Length.meter Angle.pi Angle.zero
  let dAdt = Curve2d.yCoordinate curve * VectorCurve2d.xComponent (Curve2d.derivative curve)
  let areaEstimate = Curve1d.integral dAdt
  let expectedArea = Area.squareMeters (Float.pi / 2.0)
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

smallestBy :: Tolerance Meters => Test
smallestBy = Test.check 100 "smallestBy" Test.do
  valuesAndEstimates <- dummyEstimates
  let smallestValue = Qty.smallest (NonEmpty.map Pair.first valuesAndEstimates)
  let smallestValueFromEstimates = Pair.first (Estimate.smallestBy Pair.second valuesAndEstimates)
  Test.expect (smallestValueFromEstimates == smallestValue)

largestBy :: Tolerance Meters => Test
largestBy = Test.check 100 "largestBy" Test.do
  valuesAndEstimates <- dummyEstimates
  let largestValue = Qty.largest (NonEmpty.map Pair.first valuesAndEstimates)
  let largestValueFromEstimates = Pair.first (Estimate.largestBy Pair.second valuesAndEstimates)
  Test.expect (largestValueFromEstimates == largestValue)

pickMinimumBy :: Tolerance Meters => Test
pickMinimumBy = Test.check 100 "pickMinimumBy" Test.do
  valuesAndEstimates <- dummyEstimates
  let (minPair, remainingPairs) = Estimate.pickMinimumBy Pair.second valuesAndEstimates
  let minValue = Pair.first minPair
  let remainingValues = List.map Pair.first remainingPairs
  let originalValues = NonEmpty.map Pair.first valuesAndEstimates
  let minValueIsCorrect = List.allSatisfy (>= minValue) remainingValues
  let allValuesArePresent = NonEmpty.sort originalValues == NonEmpty.sort (minValue :| remainingValues)
  Test.expect (minValueIsCorrect && allValuesArePresent)

pickMaximumBy :: Tolerance Meters => Test
pickMaximumBy = Test.check 100 "pickMaximumBy" Test.do
  valuesAndEstimates <- dummyEstimates
  let (maxPair, remainingPairs) = Estimate.pickMaximumBy Pair.second valuesAndEstimates
  let maxValue = Pair.first maxPair
  let remainingValues = List.map Pair.first remainingPairs
  let originalValues = NonEmpty.map Pair.first valuesAndEstimates
  let maxValueIsCorrect = List.allSatisfy (<= maxValue) remainingValues
  let allValuesArePresent = NonEmpty.sort originalValues == NonEmpty.sort (maxValue :| remainingValues)
  Test.expect (maxValueIsCorrect && allValuesArePresent)

pickSmallestBy :: Tolerance Meters => Test
pickSmallestBy = Test.check 100 "pickSmallestBy" Test.do
  valuesAndEstimates <- dummyEstimates
  let (smallestPair, remainingPairs) = Estimate.pickSmallestBy Pair.second valuesAndEstimates
  let smallestValue = Pair.first smallestPair
  let remainingValues = List.map Pair.first remainingPairs
  let originalValues = NonEmpty.map Pair.first valuesAndEstimates
  let smallestValueIsCorrect = List.allSatisfy (\value -> Qty.abs value >= Qty.abs smallestValue) remainingValues
  let allValuesArePresent = NonEmpty.sort originalValues == NonEmpty.sort (smallestValue :| remainingValues)
  Test.expect (smallestValueIsCorrect && allValuesArePresent)
    |> Test.output "smallestValueIsCorrect" smallestValueIsCorrect
    |> Test.output "allValuesArePresent" allValuesArePresent
    |> Test.output "smallestValue" smallestValue
    |> Test.output "remainingValues" remainingValues

pickLargestBy :: Tolerance Meters => Test
pickLargestBy = Test.check 100 "pickLargestBy" Test.do
  valuesAndEstimates <- dummyEstimates
  let (largestPair, remainingPairs) = Estimate.pickLargestBy Pair.second valuesAndEstimates
  let largestValue = Pair.first largestPair
  let remainingValues = List.map Pair.first remainingPairs
  let originalValues = NonEmpty.map Pair.first valuesAndEstimates
  let largestValueIsCorrect = List.allSatisfy (\value -> Qty.abs value <= Qty.abs largestValue) remainingValues
  let allValuesArePresent = NonEmpty.sort originalValues == NonEmpty.sort (largestValue :| remainingValues)
  Test.expect (largestValueIsCorrect && allValuesArePresent)
