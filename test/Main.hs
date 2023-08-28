module Main (main) where

import Length qualified
import OpenSolid
import Task qualified
import Test (Test)
import Test qualified
import Tests.Curve1d qualified
import Tests.Curve2d qualified
import Tests.Direction2d qualified
import Tests.Estimate qualified
import Tests.List qualified
import Tests.NonEmpty qualified
import Tests.Parameter1d qualified
import Tests.Qty qualified
import Tests.Range qualified
import Tests.Region2d qualified
import Tests.VectorBox3d qualified

tests :: List Test
tests =
  [ Test.group "Parameter1d" Tests.Parameter1d.tests
  , Test.group "Qty" Tests.Qty.tests
  , Test.group "Range" Tests.Range.tests
  , Test.group "VectorBox3d" Tests.VectorBox3d.tests
  , Test.group "Curve1d" Tests.Curve1d.tests
  , Test.group "Curve2d" Tests.Curve2d.tests
  , Test.group "Estimate" Tests.Estimate.tests
  , Test.group "List" Tests.List.tests
  , Test.group "NonEmpty" Tests.NonEmpty.tests
  , Test.group "Region2d" Tests.Region2d.tests
  , Test.group "Direction2d" Tests.Direction2d.tests
  ]
 where
  ?tolerance = Length.meters 1e-9

main :: IO ()
main = Task.toIO (Test.run tests)
