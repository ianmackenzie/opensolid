module Tests.Estimate (tests) where

import Estimate (Estimate, IsEstimate)
import Estimate qualified
import Length (Length)
import Length qualified
import NonEmpty qualified
import OpenSolid
import Parameter1d qualified
import Qty qualified
import Random (Generator)
import Random qualified
import Range (Range (Range))
import Range qualified
import Test (Test)
import Test qualified
import Tests.Random qualified as Random
import Units (Meters)

tests :: List Test
tests =
  [ smallest
  , largest
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
  Test.expect valid $
    [ Test.show "values" (Test.lines values)
    , Test.show "smallestValue" smallestValue
    , Test.show "initial bounds list" (Test.lines bounds)
    , Test.show "finalBounds" finalBounds
    ]
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
  Test.expect valid $
    [ Test.show "values" (Test.lines values)
    , Test.show "largestValue" largestValue
    , Test.show "initial bounds list" (Test.lines bounds)
    , Test.show "finalBounds" finalBounds
    ]
 where
  ?tolerance = Length.meters 1e-9
