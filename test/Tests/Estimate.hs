module Tests.Estimate (tests) where

import Angle qualified
import Arc2d qualified
import Area qualified
import Curve1d qualified
import Curve2d qualified
import Estimate (Estimate, IsEstimate)
import Estimate qualified
import Float qualified
import Length (Length)
import Length qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Pair qualified
import Parameter1d qualified
import Point2d qualified
import Qty qualified
import Random (Generator)
import Random qualified
import Random.Shuffle qualified
import Range (Range (Range))
import Range qualified
import Test (Test)
import Test qualified
import Tests.Random qualified as Random
import Units (Meters)
import VectorCurve2d qualified

tests :: List Test
tests =
  [ smallest
  , largest
  , area
  , minimumBy
  , maximumBy
  , smallestBy
  , largestBy
  , takeMinimumBy
  ]

data DummyEstimate = DummyEstimate Length (Range Meters)

instance IsEstimate DummyEstimate Meters where
  boundsImpl (DummyEstimate _ range) = range
  refineImpl (DummyEstimate value (Range low high)) =
    let refinedRange = Range.from (Qty.midpoint low value) (Qty.midpoint value high)
     in Estimate.wrap (DummyEstimate value refinedRange)

dummyEstimate :: Generator (Length, Estimate Meters)
dummyEstimate = do
  range <- Range.generator Random.length
  t <- Parameter1d.generator
  let value = Range.interpolate range t
  Random.return (value, Estimate.wrap (DummyEstimate value range))

duplicatedDummyEstimates :: Generator (NonEmpty (Length, Estimate Meters))
duplicatedDummyEstimates = do
  pair <- dummyEstimate
  numPairs <- Random.int 1 3
  Random.return (pair :| List.repeat (numPairs - 1) pair)

dummyEstimates :: Generator (NonEmpty (Length, Estimate Meters))
dummyEstimates =
  Random.nonEmpty 10 duplicatedDummyEstimates
    |> Random.map NonEmpty.concat
    |> Random.Shuffle.nonEmpty

check :: Tolerance Meters => Estimate Meters -> Qty Meters -> (Bool, Range Meters)
check estimate value
  | Range.includes value bounds =
      if Range.width bounds ~= Qty.zero
        then (True, bounds)
        else check (Estimate.refine estimate) value
  | otherwise = (False, bounds)
 where
  bounds = Estimate.bounds estimate

smallest :: Test
smallest = Test.check 100 "smallest" $ do
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
 where
  ?tolerance = Length.meters 1e-9

largest :: Test
largest = Test.check 100 "largest" $ do
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
 where
  ?tolerance = Length.meters 1e-9

resolvesTo :: Tolerance units => Qty units -> Estimate units -> Result Text Bool
resolvesTo value estimate
  | currentBounds ~= value = Ok True
  | not (Range.approximatelyIncludes value currentBounds) = Ok False
  | otherwise =
      let refinedEstimate = Estimate.refine estimate
          refinedBounds = Estimate.bounds refinedEstimate
       in if Range.width refinedBounds < Range.width currentBounds
            then resolvesTo value refinedEstimate
            else Error "Refined bounds are not narrower than original bounds"
 where
  currentBounds = Estimate.bounds estimate

area :: Test
area = Test.verify "area" $ do
  curve <-
    let ?tolerance = Length.meters 1e-9
     in Arc2d.with
          [ Arc2d.CenterPoint Point2d.origin
          , Arc2d.StartAngle (Angle.degrees 180.0)
          , Arc2d.SweptAngle (Angle.degrees -180.0)
          , Arc2d.Radius (Length.meters 1.0)
          ]
  let dAdt = Curve2d.yCoordinate curve * VectorCurve2d.xComponent (Curve2d.derivative curve)
  let areaEstimate = Curve1d.integral dAdt
  areaIsCorrect <-
    let ?tolerance = Area.squareMeters 1e-6
     in resolvesTo (Area.squareMeters (Float.pi / 2.0)) areaEstimate
  Test.expect areaIsCorrect

minimumBy :: Test
minimumBy = Test.check 100 "minimumBy" $ do
  valuesAndEstimates <- dummyEstimates
  let minimumValue = NonEmpty.minimumOf Pair.first valuesAndEstimates
  let minimumValueFromEstimates = Pair.first (Estimate.minimumBy Pair.second valuesAndEstimates)
  Test.expect (minimumValueFromEstimates == minimumValue)
 where
  ?tolerance = Length.meters 1e-9

maximumBy :: Test
maximumBy = Test.check 100 "maximumBy" $ do
  valuesAndEstimates <- dummyEstimates
  let maximumValue = NonEmpty.maximumOf Pair.first valuesAndEstimates
  let maximumValueFromEstimates = Pair.first (Estimate.maximumBy Pair.second valuesAndEstimates)
  Test.expect (maximumValueFromEstimates == maximumValue)
 where
  ?tolerance = Length.meters 1e-9

smallestBy :: Test
smallestBy = Test.check 100 "smallestBy" $ do
  valuesAndEstimates <- dummyEstimates
  let smallestValue = Qty.smallest (NonEmpty.map Pair.first valuesAndEstimates)
  let smallestValueFromEstimates = Pair.first (Estimate.smallestBy Pair.second valuesAndEstimates)
  Test.expect (smallestValueFromEstimates == smallestValue)
 where
  ?tolerance = Length.meters 1e-9

largestBy :: Test
largestBy = Test.check 100 "largestBy" $ do
  valuesAndEstimates <- dummyEstimates
  let largestValue = Qty.largest (NonEmpty.map Pair.first valuesAndEstimates)
  let largestValueFromEstimates = Pair.first (Estimate.largestBy Pair.second valuesAndEstimates)
  Test.expect (largestValueFromEstimates == largestValue)
 where
  ?tolerance = Length.meters 1e-9

takeMinimumBy :: Test
takeMinimumBy = Test.check 100 "takeMinimumBy" $ do
  valuesAndEstimates <- dummyEstimates
  let (minPair, remainingPairs) = Estimate.takeMinimumBy Pair.second valuesAndEstimates
  let minValue = Pair.first minPair
  let remainingValues = List.map Pair.first remainingPairs
  let originalValues = NonEmpty.map Pair.first valuesAndEstimates
  let minValueIsCorrect = List.all (>= minValue) remainingValues
  let allValuesArePresent = NonEmpty.sort originalValues == NonEmpty.sort (minValue :| remainingValues)
  Test.expect (minValueIsCorrect && allValuesArePresent)
 where
  ?tolerance = Length.meters 1e-9
