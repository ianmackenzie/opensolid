module OpenSolid.Body3D.HalfEdge
  ( HalfEdge (..)
  , Id (..)
  , bounds
  , curve
  , uvCurve
  , findMatingHalfEdges
  )
where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import OpenSolid.Body3D.Ids (BoundaryId, CurveId, SurfaceId)
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve3D (Curve3D)
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Set3D (Set3D)
import OpenSolid.Set3D qualified as Set3D
import OpenSolid.SurfaceCurve3D (SurfaceCurve3D)
import OpenSolid.SurfaceCurve3D qualified as SurfaceCurve3D

data HalfEdge space = HalfEdge
  { id :: Id
  , surfaceCurve :: SurfaceCurve3D space
  , uniformParameterization :: Curve1D Unitless
  }

-- | ID of a half-edge (a boundary curve of a boundary surface) within a body
data Id = Id
  { surfaceId :: SurfaceId
  , boundaryId :: BoundaryId
  , curveId :: CurveId
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

bounds :: HalfEdge space -> Bounds3D space
bounds halfEdge = SurfaceCurve3D.bounds halfEdge.surfaceCurve

curve :: HalfEdge space -> Curve3D space
curve halfEdge = SurfaceCurve3D.curve halfEdge.surfaceCurve

uvCurve :: HalfEdge space -> Curve2D Unitless
uvCurve halfEdge = SurfaceCurve3D.uvCurve halfEdge.surfaceCurve

findMatingHalfEdges ::
  Tolerance Meters =>
  Set3D space (HalfEdge space) ->
  HalfEdge space ->
  List (HalfEdge space)
findMatingHalfEdges halfEdgeSet halfEdge = do
  let halfEdgeBounds = bounds halfEdge
  halfEdgeSet & Set3D.cull (intersects halfEdgeBounds) & List.filter (isMateOf halfEdge)

isMateOf :: Tolerance Meters => HalfEdge space -> HalfEdge space -> Bool
isMateOf halfEdge1 halfEdge2 =
  halfEdge1.id /= halfEdge2.id && do
    Parameter.samples & NonEmpty.all \r1 -> do
      let r2 = 1.0 - r1
      let t1 = Curve1D.value halfEdge1.uniformParameterization r1
      let t2 = Curve1D.value halfEdge2.uniformParameterization r2
      let point1 = Curve3D.point (curve halfEdge1) t1
      let point2 = Curve3D.point (curve halfEdge2) t2
      point1 ~= point2
