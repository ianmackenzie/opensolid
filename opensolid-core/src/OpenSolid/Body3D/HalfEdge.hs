module OpenSolid.Body3D.HalfEdge
  ( HalfEdge (..)
  , Id (..)
  , id
  , bounds
  , uvBounds
  , surfaceCurve
  , curve
  , uvCurve
  , findMatingHalfEdges
  )
where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import OpenSolid.Bag3D (Bag3D)
import OpenSolid.Body3D.Ids (BoundaryId, CurveId, SurfaceId)
import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve3D (Curve3D)
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude hiding (id)
import OpenSolid.Set3D (Set3D)
import OpenSolid.Set3D qualified as Set3D
import OpenSolid.SurfaceCurve3D (SurfaceCurve3D)
import OpenSolid.SurfaceCurve3D qualified as SurfaceCurve3D
import OpenSolid.UvBounds (UvBounds)

data HalfEdge space = HalfEdge
  { id :: Id
  , surfaceCurve :: SurfaceCurve3D space
  }

instance space1 ~ space2 => Bounded (HalfEdge space1) (Bounds3D space2) where
  {-# INLINE bounds #-}
  bounds = bounds

instance Bounded (HalfEdge space) UvBounds where
  {-# INLINE bounds #-}
  bounds = uvBounds

-- | ID of a half-edge (a boundary curve of a boundary surface) within a body
data Id = Id
  { surfaceId :: SurfaceId
  , boundaryId :: BoundaryId
  , curveId :: CurveId
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

id :: HalfEdge space -> Id
id = (.id)

surfaceCurve :: HalfEdge space -> SurfaceCurve3D space
surfaceCurve = (.surfaceCurve)

bounds :: HalfEdge space -> Bounds3D space
bounds = SurfaceCurve3D.bounds . surfaceCurve

uvBounds :: HalfEdge space -> UvBounds
uvBounds = SurfaceCurve3D.uvBounds . surfaceCurve

curve :: HalfEdge space -> Curve3D space
curve = SurfaceCurve3D.curve . surfaceCurve

uvCurve :: HalfEdge space -> Curve2D Unitless
uvCurve = SurfaceCurve3D.uvCurve . surfaceCurve

findMatingHalfEdges ::
  Tolerance Meters =>
  Set3D space (HalfEdge space) ->
  HalfEdge space ->
  Bag3D space (HalfEdge space)
findMatingHalfEdges halfEdgeSet halfEdge = do
  let halfEdgeBounds = bounds halfEdge
  halfEdgeSet & Set3D.filter (^ halfEdgeBounds) (isMateOf halfEdge)

isMateOf :: Tolerance Meters => HalfEdge space -> HalfEdge space -> Bool
isMateOf halfEdge1 halfEdge2 =
  halfEdge1.id /= halfEdge2.id && matingCurves (curve halfEdge1) (curve halfEdge2)

matingCurves :: Tolerance Meters => Curve3D space -> Curve3D space -> Bool
matingCurves curve1 curve2 =
  Curve3D.length curve1 ~= Curve3D.length curve2 && do
    Parameter.samples & NonEmpty.all \r1 -> do
      let r2 = 1.0 - r1
      let point1 = Curve3D.atUniform r1 curve1
      let point2 = Curve3D.atUniform r2 curve2
      point1 ~= point2
