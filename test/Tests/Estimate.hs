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
import NonEmpty qualified
import OpenSolid
import Pair qualified
import Parameter1d qualified
import Point2d qualified
import Qty qualified
import Random (Generator)
import Random qualified
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
  valuesAndEstimates <- Random.nonEmpty 5 dummyEstimate
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
  valuesAndEstimates <- Random.nonEmpty 5 dummyEstimate
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
  valuesAndEstimates <- Random.nonEmpty 10 dummyEstimate
  -- Duplicate every item in the list,
  -- so that we make sure Estimate.minimumBy converges
  -- even if there are duplicate entries
  -- (instead of trying to refine infinitely)
  let duplicatedValuesAndEstimates = valuesAndEstimates ++ valuesAndEstimates
  let minimumValue = NonEmpty.minimumOf Pair.first valuesAndEstimates
  let minimumValueFromEstimates =
        Pair.first (Estimate.minimumBy Pair.second duplicatedValuesAndEstimates)
  Test.expect (minimumValueFromEstimates == minimumValue)
 where
  ?tolerance = Length.meters 1e-9

maximumBy :: Test
maximumBy = Test.check 100 "maximumBy" $ do
  valuesAndEstimates <- Random.nonEmpty 10 dummyEstimate
  -- Duplicate every item in the list,
  -- so that we make sure Estimate.maximumBy converges
  -- even if there are duplicate entries
  -- (instead of trying to refine infinitely)
  let duplicatedValuesAndEstimates = valuesAndEstimates ++ valuesAndEstimates
  let maximumValue = NonEmpty.maximumOf Pair.first valuesAndEstimates
  let maximumValueFromEstimates =
        Pair.first (Estimate.maximumBy Pair.second duplicatedValuesAndEstimates)
  Test.expect (maximumValueFromEstimates == maximumValue)
 where
  ?tolerance = Length.meters 1e-9

smallestBy :: Test
smallestBy = Test.check 100 "smallestBy" $ do
  valuesAndEstimates <- Random.nonEmpty 10 dummyEstimate
  -- Duplicate every item in the list,
  -- so that we make sure Estimate.smallestBy converges
  -- even if there are duplicate entries
  -- (instead of trying to refine infinitely)
  let duplicatedValuesAndEstimates = valuesAndEstimates ++ valuesAndEstimates
  let smallestValue = Qty.smallest (NonEmpty.map Pair.first valuesAndEstimates)
  let smallestValueFromEstimates =
        Pair.first (Estimate.smallestBy Pair.second duplicatedValuesAndEstimates)
  Test.expect (smallestValueFromEstimates == smallestValue)
 where
  ?tolerance = Length.meters 1e-9

largestBy :: Test
largestBy = Test.check 100 "largestBy" $ do
  valuesAndEstimates <- Random.nonEmpty 10 dummyEstimate
  -- Duplicate every item in the list,
  -- so that we make sure Estimate.largestBy converges
  -- even if there are duplicate entries
  -- (instead of trying to refine infinitely)
  let duplicatedValuesAndEstimates = valuesAndEstimates ++ valuesAndEstimates
  let largestValue = Qty.largest (NonEmpty.map Pair.first valuesAndEstimates)
  let largestValueFromEstimates =
        Pair.first (Estimate.largestBy Pair.second duplicatedValuesAndEstimates)
  Test.expect (largestValueFromEstimates == largestValue)
 where
  ?tolerance = Length.meters 1e-9
