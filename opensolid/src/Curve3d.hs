-- Avoid errors when running Fourmolu
{-# LANGUAGE GHC2021 #-}

module Curve3d
  ( Curve3d
  , Interface (..)
  , DegenerateCurve (DegenerateCurve)
  , new
  , constant
  , startPoint
  , endPoint
  , pointOn
  , segmentBounds
  , derivative
  , reverse
  , bounds
  )
where

import Bounds3d (Bounds3d)
import Curve3d.Internal (Interface (..))
import Curve3d.Internal qualified as Internal
import Error qualified
import OpenSolid
import Point3d (Point3d)
import Range (Range)
import VectorCurve3d (VectorCurve3d)

type Curve3d (coordinateSystem :: CoordinateSystem) = Internal.Curve3d coordinateSystem

data DegenerateCurve = DegenerateCurve deriving (Eq, Show, Error.Message)

new :: Interface curve (space @ units) => curve -> Curve3d (space @ units)
new = Internal.Curve

constant :: Point3d (space @ units) -> Curve3d (space @ units)
constant = Internal.Constant

startPoint :: Curve3d (space @ units) -> Point3d (space @ units)
startPoint = Internal.startPoint

endPoint :: Curve3d (space @ units) -> Point3d (space @ units)
endPoint = Internal.endPoint

pointOn :: Curve3d (space @ units) -> Float -> Point3d (space @ units)
pointOn = Internal.pointOn

segmentBounds :: Curve3d (space @ units) -> Range Unitless -> Bounds3d (space @ units)
segmentBounds = Internal.segmentBounds

derivative :: Curve3d (space @ units) -> VectorCurve3d (space @ units)
derivative = Internal.derivative

reverse :: Curve3d (space @ units) -> Curve3d (space @ units)
reverse = Internal.reverse

bounds :: Curve3d (space @ units) -> Bounds3d (space @ units)
bounds = Internal.bounds
