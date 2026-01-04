module OpenSolid.Curve3D.IntersectionPointWithSurface
  ( Kind (Crossing, Tangent)
  , IntersectionPointWithSurface (IntersectionPointWithSurface, kind, t, uv)
  , crossing
  , tangent
  )
where

import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint)

data Kind = Crossing Sign | Tangent deriving (Eq, Ord, Show)

data IntersectionPointWithSurface = IntersectionPointWithSurface
  { t :: Number
  , uv :: UvPoint
  , kind :: Kind
  }
  deriving (Eq, Ord, Show)

instance ApproximateEquality IntersectionPointWithSurface Unitless where
  first ~= second = first.kind == second.kind && first.t ~= second.t && first.uv ~= second.uv

crossing :: Number -> UvPoint -> Sign -> IntersectionPointWithSurface
crossing t uv sign = IntersectionPointWithSurface{t, uv, kind = Crossing sign}

tangent :: Number -> UvPoint -> IntersectionPointWithSurface
tangent t uv = IntersectionPointWithSurface{t, uv, kind = Tangent}
