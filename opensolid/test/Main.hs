module Main (main) where

import Length qualified
import OpenSolid
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
import Tests.String qualified
import Tests.VectorBounds2d qualified
import Tests.VectorBounds3d qualified
import Tolerance qualified

tests :: List Test
tests =
  Tolerance.using (Length.meters 1e-9) $
    [ Test.group "Parameter" Tests.Parameter.tests
    , Test.group "Qty" Tests.Qty.tests
    , Test.group "Range" Tests.Range.tests
    , Test.group "VectorBounds3d" Tests.VectorBounds3d.tests
    , Test.group "Curve1d" Tests.Curve1d.tests
    , Test.group "Curve2d" Tests.Curve2d.tests
    , Test.group "Estimate" Tests.Estimate.tests
    , Test.group "List" Tests.List.tests
    , Test.group "NonEmpty" Tests.NonEmpty.tests
    , Test.group "Region2d" Tests.Region2d.tests
    , Test.group "Direction2d" Tests.Direction2d.tests
    , Test.group "Arc2d" Tests.Arc2d.tests
    , Test.group "Map" Tests.Map.tests
    , Test.group "DivMod" Tests.DivMod.tests
    , Test.group "Float" Tests.Float.tests
    , Test.group "String" Tests.String.tests
    , Test.group "Bounds2d" Tests.Bounds2d.tests
    , Test.group "VectorBounds2d" Tests.VectorBounds2d.tests
    ]

main :: IO ()
main = Test.run tests
