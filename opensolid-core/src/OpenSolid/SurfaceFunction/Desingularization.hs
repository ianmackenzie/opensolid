module OpenSolid.SurfaceFunction.Desingularization (zeroU0, zeroU1, zeroV0, zeroV1) where

import OpenSolid.List qualified as List
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import {-# SOURCE #-} OpenSolid.SurfaceFunction qualified as SurfaceFunction

zeroU0 :: Tolerance units => SurfaceFunction units -> Bool
zeroU0 function =
  List.allTrue
    [SurfaceFunction.evaluate function (Point2d 0.0 v) ~= Qty.zero | v <- Parameter.samples]

zeroU1 :: Tolerance units => SurfaceFunction units -> Bool
zeroU1 function =
  List.allTrue
    [SurfaceFunction.evaluate function (Point2d 1.0 v) ~= Qty.zero | v <- Parameter.samples]

zeroV0 :: Tolerance units => SurfaceFunction units -> Bool
zeroV0 function =
  List.allTrue
    [SurfaceFunction.evaluate function (Point2d u 0.0) ~= Qty.zero | u <- Parameter.samples]

zeroV1 :: Tolerance units => SurfaceFunction units -> Bool
zeroV1 function =
  List.allTrue
    [SurfaceFunction.evaluate function (Point2d u 1.0) ~= Qty.zero | u <- Parameter.samples]
