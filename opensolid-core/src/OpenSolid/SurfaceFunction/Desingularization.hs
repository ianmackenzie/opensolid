-- | Helper functions for surface function desingularization.
module OpenSolid.SurfaceFunction.Desingularization (isZero, testPoints) where

import OpenSolid.List qualified as List
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import {-# SOURCE #-} OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.UvPoint (UvPoint)

{-| Generate a list of test points at a given fixed U or V value, varying the other parameter.

The given value should generally be either 0 or 1;
for example 'testPoints U 0.0' will generate some test points with different V values along U=0,
suitable for testing if some function is zero everywhere at U=0.
(Under the hood this calls 'Parameter.samples' to generate the varying parameter values.)
-}
testPoints :: SurfaceParameter -> Number -> List UvPoint
testPoints U uValue = [Point2d uValue v | v <- Parameter.samples]
testPoints V vValue = [Point2d u vValue | u <- Parameter.samples]

{-| Check if a given surface function is zero everywhere at a given fixed U or V value.

The given value should generally either be 0 or 1, for example 'isZero U 0.0 function'
will check if the given function is zero everywhere along U=0.
-}
isZero :: Tolerance units => SurfaceParameter -> Number -> SurfaceFunction units -> Bool
isZero parameter value function =
  List.allTrue
    [ SurfaceFunction.evaluate function testPoint ~= Quantity.zero
    | testPoint <- testPoints parameter value
    ]
