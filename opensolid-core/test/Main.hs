module Main (main) where

import OpenSolid.Length qualified as Length
import OpenSolid.Prelude
import OpenSolid.Tolerance qualified as Tolerance
import Test (Test)
import Test qualified
import Tests.Array qualified
import Tests.Bounds qualified
import Tests.Bounds2D qualified
import Tests.Bounds3D qualified
import Tests.Curve qualified
import Tests.Curve2D qualified
import Tests.Direction2D qualified
import Tests.DivMod qualified
import Tests.Estimate qualified
import Tests.List qualified
import Tests.Map qualified
import Tests.NewtonRaphson qualified
import Tests.NonEmpty qualified
import Tests.Number qualified
import Tests.Parameter qualified
import Tests.Quantity qualified
import Tests.Region2D qualified
import Tests.Set2D qualified
import Tests.Stream qualified
import Tests.SurfaceFunction qualified
import Tests.Text qualified
import Tests.VectorBounds2D qualified
import Tests.VectorBounds3D qualified

tests :: List Test
tests =
  Tolerance.using Length.nanometer $
    [ Test.group "Tests.Parameter" Tests.Parameter.tests
    , Test.group "Tests.Quantity" Tests.Quantity.tests
    , Test.group "Tests.Bounds" Tests.Bounds.tests
    , Test.group "Tests.VectorBounds3D" Tests.VectorBounds3D.tests
    , Test.group "Tests.Curve" Tests.Curve.tests
    , Test.group "Tests.Curve2D" Tests.Curve2D.tests
    , Test.group "Tests.Estimate" Tests.Estimate.tests
    , Test.group "Tests.List" Tests.List.tests
    , Test.group "Tests.NonEmpty" Tests.NonEmpty.tests
    , Test.group "Tests.Region2D" Tests.Region2D.tests
    , Test.group "Tests.Direction2D" Tests.Direction2D.tests
    , Test.group "Tests.Map" Tests.Map.tests
    , Test.group "Tests.DivMod" Tests.DivMod.tests
    , Test.group "Tests.Number" Tests.Number.tests
    , Test.group "Tests.Stream" Tests.Stream.tests
    , Test.group "Tests.Text" Tests.Text.tests
    , Test.group "Tests.Bounds2D" Tests.Bounds2D.tests
    , Test.group "Tests.VectorBounds2D" Tests.VectorBounds2D.tests
    , Test.group "Tests.Bounds3D" Tests.Bounds3D.tests
    , Test.group "Tests.Array" Tests.Array.tests
    , Test.group "Tests.Set2D" Tests.Set2D.tests
    , Test.group "Tests.SurfaceFunction" Tests.SurfaceFunction.tests
    , Test.group "Tests.NewtonRaphson" Tests.NewtonRaphson.tests
    ]

main :: IO ()
main = Test.run tests
