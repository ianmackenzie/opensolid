module OpenSolid.Surface3D.Pole
  ( Pole
  , new
  , uvCurve
  , uvEdge
  , point
  , placeIn
  , relativeTo
  )
where

import Data.Coerce qualified
import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Space qualified as Space
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvCurve (UvCurve)
import OpenSolid.UvCurve qualified as UvCurve
import OpenSolid.UvEdge (UvEdge)
import OpenSolid.UvEdge qualified as UvEdge
import OpenSolid.UvPoint (UvPoint)

data Pole space = Pole
  { uvCurve :: UvCurve
  , uvEdge :: UvEdge
  , point :: Point3D space
  }
  deriving (Show)

instance Space.Coercion (Pole space1) (Pole space2) where
  {-# INLINE coerce #-}
  coerce = Data.Coerce.coerce

instance Bounded (Pole space) UvBounds where
  {-# INLINE bounds #-}
  bounds pole = UvCurve.bounds (uvCurve pole)

instance space1 ~ space2 => Bounded (Pole space1) (Bounds3D space2) where
  {-# INLINE bounds #-}
  bounds pole = Bounds3D.constant (point pole)

instance Intersects (Pole space) (Point3D space) Meters where
  {-# INLINE (^) #-}
  pole ^ givenPoint = point pole ~= givenPoint

instance Intersects (Point3D space) (Pole space) Meters where
  {-# INLINE (^) #-}
  givenPoint ^ pole = pole ^ givenPoint

instance Intersects UvPoint (Pole space) Unitless where
  -- Note that in general "point intersects curve" can be relatively expensive
  -- (since it calls Curve.findPoint internally),
  -- but in this case there are a couple mitigating factors:
  --
  --   * The UV curve of a pole should always be a vertical or horizontal line
  --     (along U or V equal to 0 or 1),
  --     so its bounding box should be tight
  --     and therefore able to reject any non-intersecting points
  --   * If a UV point intersects the UV curve bounding box,
  --     it will generally be an *endpoint* of the UV curve,
  --     which will then cause an early fast return from Curve.findPoint
  uvPoint ^ pole = uvPoint ^ uvCurve pole

instance Intersects (Pole space) UvPoint Unitless where
  pole ^ givenPoint = givenPoint ^ pole

new :: UvCurve -> Point3D space -> Pole space
new givenUvCurve givenPoint = do
  let samplePoints = NonEmpty.map (UvCurve.pointOn givenUvCurve) Parameter.samples
  let allSamplePointsOn edge = unitless do NonEmpty.all (^ edge) samplePoints
  let detectedUvEdge
        | allSamplePointsOn UvEdge.Left = UvEdge.Left
        | allSamplePointsOn UvEdge.Right = UvEdge.Right
        | allSamplePointsOn UvEdge.Bottom = UvEdge.Bottom
        | allSamplePointsOn UvEdge.Top = UvEdge.Top
        | otherwise = error "Pole UV curve must be along U=0, U=1, V=0 or V=1"
  Pole {uvCurve = givenUvCurve, uvEdge = detectedUvEdge, point = givenPoint}

uvCurve :: Pole space -> UvCurve
uvCurve = (.uvCurve)

uvEdge :: Pole space -> UvEdge
uvEdge = (.uvEdge)

point :: Pole space -> Point3D space
point = (.point)

placeIn :: Frame3D global local -> Pole local -> Pole global
placeIn frame pole =
  Pole
    { uvCurve = pole.uvCurve
    , uvEdge = pole.uvEdge
    , point = Point3D.placeIn frame pole.point
    }

relativeTo :: Frame3D global local -> Pole global -> Pole local
relativeTo frame pole = placeIn (Frame3D.inverse frame) pole
