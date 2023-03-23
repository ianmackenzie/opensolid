module Curve2d
  ( Curve2d (Curve2d)
  , IsCurve2d
  )
where

import OpenSolid

class IsCurve2d curve space units | curve -> space, curve -> units

type role Curve2d nominal

data Curve2d (coordinateSystem :: CoordinateSystem) where
  Curve2d :: IsCurve2d curve space units => curve -> Curve2d (space @ units)
