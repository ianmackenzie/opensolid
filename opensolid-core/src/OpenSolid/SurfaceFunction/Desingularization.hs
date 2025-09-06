module OpenSolid.SurfaceFunction.Desingularization (isZero) where

import OpenSolid.List qualified as List
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import {-# SOURCE #-} OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))

isZero :: Tolerance units => SurfaceParameter -> Float -> SurfaceFunction units -> Bool
isZero parameter value function = do
  let uvPoints = case parameter of
        U -> [Point2d value v | v <- Parameter.samples]
        V -> [Point2d u value | u <- Parameter.samples]
  List.allTrue [SurfaceFunction.evaluate function uvPoint ~= Qty.zero | uvPoint <- uvPoints]
