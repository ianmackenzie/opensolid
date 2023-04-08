module Curve2d
  ( Curve2d (Curve2d)
  , IsCurve2d
  )
where

import OpenSolid

class IsCurve2d curve (coordinateSystem :: CoordinateSystem) | curve -> coordinateSystem

type role Curve2d nominal

data Curve2d (coordinateSystem :: CoordinateSystem) where
  Curve2d :: IsCurve2d curve (space @ units) => curve -> Curve2d (space @ units)
