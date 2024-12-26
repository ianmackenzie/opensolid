module Main (main) where

import Length qualified
import OpenSolid.Prelude
import Test (Test)
import Test qualified
import Tests.Arc2d qualified
import Tests.Bounds2d qualified
import Tests.Curve1d qualified
import Tests.Curve2d qualified
import Tests.Direction2d qualified
import Tests.DivMod qualified
import Tests.Estimate qualified
import Tests.Float qualified
import Tests.List qualified
import Tests.Map qualified
import Tests.NonEmpty qualified
import Tests.Parameter qualified
import Tests.Qty qualified
import Tests.Range qualified
import Tests.Region2d qualified
import Tests.Stream qualified
import Tests.Text qualified
import Tests.VectorBounds2d qualified
import Tests.VectorBounds3d qualified
import Tolerance qualified

tests :: List Test
tests =
  Tolerance.using (Length.meters 1e-9) $
    [ Test.group "Tests.Parameter" Tests.Parameter.tests
    , Test.group "Tests.Qty" Tests.Qty.tests
    , Test.group "Tests.Range" Tests.Range.tests
    , Test.group "Tests.VectorBounds3d" Tests.VectorBounds3d.tests
    , Test.group "Tests.Curve1d" Tests.Curve1d.tests
    , Test.group "Tests.Curve2d" Tests.Curve2d.tests
    , Test.group "Tests.Estimate" Tests.Estimate.tests
    , Test.group "Tests.List" Tests.List.tests
    , Test.group "Tests.NonEmpty" Tests.NonEmpty.tests
    , Test.group "Tests.Region2d" Tests.Region2d.tests
    , Test.group "Tests.Direction2d" Tests.Direction2d.tests
    , Test.group "Tests.Arc2d" Tests.Arc2d.tests
    , Test.group "Tests.Map" Tests.Map.tests
    , Test.group "Tests.DivMod" Tests.DivMod.tests
    , Test.group "Tests.Float" Tests.Float.tests
    , Test.group "Tests.Stream" Tests.Stream.tests
    , Test.group "Tests.Text" Tests.Text.tests
    , Test.group "Tests.Bounds2d" Tests.Bounds2d.tests
    , Test.group "Tests.VectorBounds2d" Tests.VectorBounds2d.tests
    ]

main :: IO ()
main = Test.run tests
