module OpenSolid.SurfacePoint3D
  ( SurfacePoint3D (Point, Pole)
  , point
  , uvBounds
  )
where

import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvBounds qualified as UvBounds
import OpenSolid.UvCurve (UvCurve)
import OpenSolid.UvCurve qualified as UvCurve
import OpenSolid.UvPoint (UvPoint)

data SurfacePoint3D space
  = Point UvPoint (Point3D space)
  | Pole (Nondegenerate UvCurve) (Point3D space)
  deriving (Show)

instance space1 ~ space2 => Bounded (SurfacePoint3D space1) (Bounds3D space2) where
  bounds = Bounds3D.constant . point

instance Bounded (SurfacePoint3D space) UvBounds where
  bounds = uvBounds

instance space1 ~ space2 => Intersects (SurfacePoint3D space1) (SurfacePoint3D space2) Meters where
  surfacePoint1 ^ surfacePoint2 = point surfacePoint1 ~= point surfacePoint2

instance space1 ~ space2 => Intersects (SurfacePoint3D space1) (Point3D space2) Meters where
  surfacePoint ^ givenPoint = point surfacePoint ~= givenPoint

instance space1 ~ space2 => Intersects (Point3D space1) (SurfacePoint3D space2) Meters where
  givenPoint ^ surfacePoint = givenPoint ~= point surfacePoint

instance space1 ~ space2 => Intersects (SurfacePoint3D space1) (Bounds3D space2) Meters where
  surfacePoint ^ bounds = point surfacePoint ^ bounds

instance space1 ~ space2 => Intersects (Bounds3D space1) (SurfacePoint3D space2) Meters where
  bounds ^ surfacePoint = bounds ^ point surfacePoint

point :: SurfacePoint3D space -> Point3D space
point (Point _ p) = p
point (Pole _ p) = p

uvBounds :: SurfacePoint3D space -> UvBounds
uvBounds (Point uvPoint _) = UvBounds.constant uvPoint
uvBounds (Pole (Nondegenerate uvCurve) _) = UvCurve.bounds uvCurve
