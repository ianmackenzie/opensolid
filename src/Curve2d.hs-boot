module Curve2d
  ( Curve2d (Curve2d)
  , IsCurve2d
  )
where

import OpenSolid

class IsCurve2d curve coordinates units | curve -> units, curve -> coordinates

type role Curve2d nominal nominal

type Curve2d :: Type -> Type -> Type
data Curve2d coordinates units = forall curve. IsCurve2d curve coordinates units => Curve2d curve
