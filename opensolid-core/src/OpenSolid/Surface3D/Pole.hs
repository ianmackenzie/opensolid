module OpenSolid.Surface3D.Pole
  ( Pole
  , IsDegenerate (IsDegenerate)
  , with
  , uvCurve
  , uvEdge
  , point
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Surface3D.Pin (Pin)
import OpenSolid.Surface3D.Pin qualified as Surface3D.Pin
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

instance Bounded (Pole space) UvBounds where
  {-# INLINE bounds #-}
  bounds pole = UvCurve.bounds (uvCurve pole)

instance space1 ~ space2 => Bounded (Pole space1) (Bounds3D space2) where
  {-# INLINE bounds #-}
  bounds pole = Bounds3D.constant (point pole)

instance At (Pole space) Number (Pin space) where
  {-# INLINE (@) #-}
  pole @ tValue = Surface3D.Pin.with {uvPoint = uvCurve pole @ tValue, point = point pole}

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

data IsDegenerate space = IsDegenerate (Pin space) deriving (Show, Err)

instance
  Composition
    ()
    (Pole space)
    (Curve1D Unitless)
    (Result (IsDegenerate space) (Pole space))
  where
  pole << parameterization =
    case unitless (uvCurve pole << parameterization) of
      Ok composedUvCurve ->
        Ok $ Pole {uvCurve = composedUvCurve, uvEdge = uvEdge pole, point = point pole}
      Err (UvCurve.IsDegenerate uvPoint) ->
        Err $ IsDegenerate (Surface3D.Pin.with {uvPoint = uvPoint, point = point pole})

with :: ("uvCurve" ::: UvCurve, "point" ::: Point3D space) -> Pole space
with params = do
  let samplePoints = params.uvCurve @ Parameter.samples
  let allSamplePointsOn edge = unitless do NonEmpty.all (^ edge) samplePoints
  let detectedUvEdge
        | allSamplePointsOn UvEdge.Left = UvEdge.Left
        | allSamplePointsOn UvEdge.Right = UvEdge.Right
        | allSamplePointsOn UvEdge.Bottom = UvEdge.Bottom
        | allSamplePointsOn UvEdge.Top = UvEdge.Top
        | otherwise = error "Pole UV curve must be along U=0, U=1, V=0 or V=1"
  Pole {uvCurve = params.uvCurve, uvEdge = detectedUvEdge, point = params.point}

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
