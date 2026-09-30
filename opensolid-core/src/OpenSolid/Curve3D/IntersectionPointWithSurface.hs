module OpenSolid.Curve3D.IntersectionPointWithSurface
  ( Kind (Crossing, Tangent)
  , IntersectionPointWithSurface
  , crossing
  , tangent
  , parameterValue
  , surfacePoint
  , kind
  )
where

import OpenSolid.Prelude
import OpenSolid.SurfacePoint3D (SurfacePoint3D)

data Kind = Crossing Sign | Tangent deriving (Eq, Show)

data IntersectionPointWithSurface space = IntersectionPointWithSurface
  { parameterValue :: Number
  , surfacePoint :: SurfacePoint3D space
  , kind :: Kind
  }
  deriving (Show)

crossing :: Number -> SurfacePoint3D space -> Sign -> IntersectionPointWithSurface space
crossing givenParameterValue givenSurfacePoint sign =
  IntersectionPointWithSurface
    { parameterValue = givenParameterValue
    , surfacePoint = givenSurfacePoint
    , kind = Crossing sign
    }

tangent :: Number -> SurfacePoint3D space -> IntersectionPointWithSurface space
tangent givenParameterValue givenSurfacePoint =
  IntersectionPointWithSurface
    { parameterValue = givenParameterValue
    , surfacePoint = givenSurfacePoint
    , kind = Tangent
    }

parameterValue :: IntersectionPointWithSurface space -> Number
parameterValue = (.parameterValue)

surfacePoint :: IntersectionPointWithSurface space -> SurfacePoint3D space
surfacePoint = (.surfacePoint)

kind :: IntersectionPointWithSurface space -> Kind
kind = (.kind)
