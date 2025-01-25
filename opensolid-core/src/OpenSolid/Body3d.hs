{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module OpenSolid.Body3d
  ( Body3d
  , boundedBy
  , toMesh
  )
where

import OpenSolid.Body3d.BoundedBy qualified as BoundedBy
import OpenSolid.Bounds2d (Bounded2d, Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Bounds3d (Bounded3d, Bounds3d)
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.ConstrainedDelaunayTriangulation qualified as CDT
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Curve3d (Curve3d)
import OpenSolid.Curve3d qualified as Curve3d
import OpenSolid.Domain1d qualified as Domain1d
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Fuzzy qualified as Fuzzy
import OpenSolid.LineSegment2d (LineSegment2d)
import OpenSolid.Linearization qualified as Linearization
import OpenSolid.List qualified as List
import OpenSolid.Mesh (Mesh)
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point3d (Point3d)
import OpenSolid.Polygon2d (Polygon2d (Polygon2d))
import OpenSolid.Polygon2d qualified as Polygon2d
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Result qualified as Result
import OpenSolid.Set2d (Set2d)
import OpenSolid.Set2d qualified as Set2d
import OpenSolid.Set3d (Set3d)
import OpenSolid.Set3d qualified as Set3d
import OpenSolid.Surface3d (Surface3d)
import OpenSolid.Surface3d qualified as Surface3d
import OpenSolid.SurfaceFunction3d (SurfaceFunction3d)
import OpenSolid.SurfaceFunction3d qualified as SurfaceFunction3d
import OpenSolid.SurfaceLinearization qualified as SurfaceLinearization
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V), UvBounds, UvCoordinates, UvPoint)
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.VectorBounds3d qualified as VectorBounds3d
import OpenSolid.VectorCurve3d (VectorCurve3d)
import OpenSolid.VectorCurve3d qualified as VectorCurve3d
import OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)
import OpenSolid.VectorSurfaceFunction3d qualified as VectorSurfaceFunction3d
import OpenSolid.Vertex2d (Vertex2d)
import OpenSolid.Vertex2d qualified as Vertex2d

type role Body3d nominal

newtype Body3d (coordinateSystem :: CoordinateSystem)
  = Body3d (NonEmpty (BoundarySurface coordinateSystem))

data BoundarySurface (coordinateSystem :: CoordinateSystem) where
  BoundarySurface ::
    { surfaceId :: Int
    , surfaceFunctions :: SurfaceFunctions (space @ units)
    , edgeLoops :: NonEmpty (NonEmpty (Edge (space @ units)))
    } ->
    BoundarySurface (space @ units)

data Edge (coordinateSystem :: CoordinateSystem) where
  Edge ::
    { startPoint :: Point3d (space @ units)
    , halfEdge :: HalfEdge (space @ units)
    , matingEdge :: HalfEdge (space @ units)
    } ->
    Edge (space @ units)

data HalfEdge (coordinateSystem :: CoordinateSystem) where
  HalfEdge ::
    { surfaceId :: Int
    , surfaceFunctions :: SurfaceFunctions (space @ units)
    , uvCurve :: Curve2d UvCoordinates -- UV curve parameterized by 3D arc length
    , curve3d :: Curve3d (space @ units) -- Arc length parameterized 3D curve
    , bounds :: Bounds3d (space @ units) -- Bounds on 3D curve
    } ->
    HalfEdge (space @ units)

instance Bounded3d (HalfEdge (space @ units)) (space @ units) where
  bounds HalfEdge{bounds} = bounds

data SurfaceFunctions (coordinateSystem :: CoordinateSystem) where
  SurfaceFunctions ::
    { f :: SurfaceFunction3d (space @ units)
    , fuu :: VectorSurfaceFunction3d (space @ units)
    , fuv :: VectorSurfaceFunction3d (space @ units)
    , fvv :: VectorSurfaceFunction3d (space @ units)
    } ->
    SurfaceFunctions (space @ units)

data SurfaceWithHalfEdges (coordinateSystem :: CoordinateSystem) where
  SurfaceWithHalfEdges ::
    { surfaceId :: Int
    , surfaceFunctions :: SurfaceFunctions (space @ units)
    , halfEdgeLoops :: NonEmpty (NonEmpty (HalfEdge (space @ units)))
    } ->
    SurfaceWithHalfEdges (space @ units)

data Corner (coordinateSystem :: CoordinateSystem) where
  Corner :: {surfaceId :: Int, point :: Point3d (space @ units)} -> Corner (space @ units)

instance Bounded3d (Corner (space @ units)) (space @ units) where
  bounds Corner{point} = Bounds3d.constant point

boundedBy ::
  Tolerance units =>
  List (Surface3d (space @ units)) ->
  Result BoundedBy.Error (Body3d (space @ units))
boundedBy [] = Failure BoundedBy.EmptyBody
boundedBy (NonEmpty surfaces) = Result.do
  surfacesWithHalfEdges <- Result.collect toSurfaceWithHalfEdges (NonEmpty.indexed surfaces)
  let halfEdges = NonEmpty.collect getAllHalfEdges surfacesWithHalfEdges
  let halfEdgeSet = Set3d.fromNonEmpty halfEdges
  let corners = NonEmpty.map halfEdgeStartPoint halfEdges
  let cornerSet = Set3d.fromNonEmpty corners
  boundarySurfaces <- Result.collect (toBoundarySurface cornerSet halfEdgeSet) surfacesWithHalfEdges
  Success (Body3d boundarySurfaces)

getAllHalfEdges :: SurfaceWithHalfEdges (space @ units) -> NonEmpty (HalfEdge (space @ units))
getAllHalfEdges SurfaceWithHalfEdges{halfEdgeLoops} = NonEmpty.concat halfEdgeLoops

halfEdgeStartPoint :: HalfEdge (space @ units) -> Corner (space @ units)
halfEdgeStartPoint HalfEdge{surfaceId, curve3d} = Corner surfaceId (Curve3d.startPoint curve3d)

toSurfaceWithHalfEdges ::
  Tolerance units =>
  (Int, Surface3d (space @ units)) ->
  Result BoundedBy.Error (SurfaceWithHalfEdges (space @ units))
toSurfaceWithHalfEdges (surfaceId, surface) = do
  let surfaceFunctions = toSurfaceFunctions (Surface3d.function surface)
  let surfaceDomain = Surface3d.domain surface
  let loops = Region2d.outerLoop surfaceDomain :| Region2d.innerLoops surfaceDomain
  Result.map (SurfaceWithHalfEdges surfaceId surfaceFunctions) $
    Result.collect (Result.collect (surfaceHalfEdge surfaceId surfaceFunctions)) loops

surfaceHalfEdge ::
  Tolerance units =>
  Int ->
  SurfaceFunctions (space @ units) ->
  Curve2d UvCoordinates ->
  Result BoundedBy.Error (HalfEdge (space @ units))
surfaceHalfEdge surfaceId surfaceFunctions uvCurve = do
  let SurfaceFunctions{f} = surfaceFunctions
  let curve3d = f . uvCurve
  case Curve3d.arcLengthParameterization curve3d of
    Success (parameterization, _) ->
      Success $
        HalfEdge
          surfaceId
          surfaceFunctions
          (uvCurve . parameterization)
          (curve3d . parameterization)
          (Curve3d.bounds curve3d)
    Failure Curve3d.HasDegeneracy ->
      Failure BoundedBy.BoundaryCurveHasDegeneracy

toBoundarySurface ::
  Tolerance units =>
  Set3d (Corner (space @ units)) (space @ units) ->
  Set3d (HalfEdge (space @ units)) (space @ units) ->
  SurfaceWithHalfEdges (space @ units) ->
  Result BoundedBy.Error (BoundarySurface (space @ units))
toBoundarySurface cornerSet halfEdgeSet surfaceWithHalfEdges = Result.do
  let SurfaceWithHalfEdges{surfaceId, surfaceFunctions, halfEdgeLoops} = surfaceWithHalfEdges
  edgeLoops <- Result.collect (Result.collect (toEdge cornerSet halfEdgeSet)) halfEdgeLoops
  Success BoundarySurface{surfaceId, surfaceFunctions, edgeLoops}

toSurfaceFunctions :: SurfaceFunction3d (space @ units) -> SurfaceFunctions (space @ units)
toSurfaceFunctions f = do
  let fu = SurfaceFunction3d.derivative U f
  let fv = SurfaceFunction3d.derivative V f
  let fuu = VectorSurfaceFunction3d.derivative U fu
  let fuv = VectorSurfaceFunction3d.derivative V fu
  let fvv = VectorSurfaceFunction3d.derivative V fv
  SurfaceFunctions{f, fuu, fuv, fvv}

getCornerPoint ::
  Tolerance units =>
  Point3d (space @ units) ->
  Set3d (Corner (space @ units)) (space @ units) ->
  Result BoundedBy.Error (Point3d (space @ units))
getCornerPoint searchPoint cornerSet =
  case Set3d.filter (Bounds3d.constant searchPoint) cornerSet of
    [] -> internalError "getCorner should always find at least one corner (the given point itself)"
    NonEmpty candidates -> do
      let Corner{point} = NonEmpty.minimumBy (\Corner{surfaceId} -> surfaceId) candidates
      Success point

toEdge ::
  Tolerance units =>
  Set3d (Corner (space @ units)) (space @ units) ->
  Set3d (HalfEdge (space @ units)) (space @ units) ->
  HalfEdge (space @ units) ->
  Result BoundedBy.Error (Edge (space @ units))
toEdge cornerSet halfEdgeSet halfEdge = Result.do
  let HalfEdge{curve3d} = halfEdge
  startPoint <- getCornerPoint (Curve3d.startPoint curve3d) cornerSet
  let matingEdgeCandidates = Set3d.filter (Bounds3d.bounds halfEdge) halfEdgeSet
  case List.filter (isMatingEdge halfEdge) matingEdgeCandidates of
    [] -> Failure BoundedBy.BoundaryHasGaps
    List.One matingEdge -> Success (Edge startPoint halfEdge matingEdge)
    List.TwoOrMore -> Failure BoundedBy.BoundaryIntersectsItself

isMatingEdge :: Tolerance units => HalfEdge (space @ units) -> HalfEdge (space @ units) -> Bool
isMatingEdge HalfEdge{curve3d = curve1} HalfEdge{curve3d = curve2} = do
  let matchingPoints t1 = Curve3d.evaluate curve1 t1 ~= Curve3d.evaluate curve2 (1.0 - t1)
  List.allTrue [matchingPoints t | t <- Parameter.samples]

data Vertex (coordinateSystem :: CoordinateSystem) where
  Vertex :: UvPoint -> Point3d (space @ units) -> Vertex (space @ units)

instance Bounded2d (Vertex (space @ units)) UvCoordinates where
  bounds (Vertex uvPoint _) = Bounds2d.constant uvPoint

instance Vertex2d (Vertex (space @ units)) UvCoordinates where
  position (Vertex uvPoint _) = uvPoint

edgeVertices :: Qty units -> Edge (space @ units) -> NonEmpty (Vertex (space @ units))
edgeVertices accuracy Edge{startPoint, halfEdge, matingEdge} = do
  let HalfEdge{uvCurve, curve3d} = halfEdge
  let secondDerivative3d = VectorCurve3d.derivative (Curve3d.derivative curve3d)
  let predicate = linearizationPredicate accuracy halfEdge matingEdge secondDerivative3d
  let innerParameterValues = Domain1d.innerSamplingPoints predicate
  let startVertex = Vertex (Curve2d.startPoint uvCurve) startPoint
  let toVertex tValue = Vertex (Curve2d.evaluate uvCurve tValue) (Curve3d.evaluate curve3d tValue)
  startVertex :| List.map toVertex innerParameterValues

linearizationPredicate ::
  Qty units ->
  HalfEdge (space @ units) ->
  HalfEdge (space @ units) ->
  VectorCurve3d (space @ units) ->
  Range Unitless ->
  Bool
linearizationPredicate accuracy halfEdge1 halfEdge2 secondDerivative3d subdomain = do
  let HalfEdge{surfaceFunctions = surfaceFunctions1, uvCurve = uvCurve1} = halfEdge1
  let HalfEdge{surfaceFunctions = surfaceFunctions2, uvCurve = uvCurve2} = halfEdge2
  let curveSecondDerivativeBounds = VectorCurve3d.evaluateBounds secondDerivative3d subdomain
  let curveSecondDerivativeMagnitude = VectorBounds3d.magnitude curveSecondDerivativeBounds
  let curveMaxDomainSize = Linearization.maxDomainSize accuracy curveSecondDerivativeMagnitude
  let uvBounds1 = Curve2d.evaluateBounds uvCurve1 subdomain
  let surfaceMaxDomainSize1 = surfaceMaxDomainSize accuracy surfaceFunctions1 uvBounds1
  let uvBounds2 = Curve2d.evaluateBounds uvCurve2 (1.0 - subdomain)
  let surfaceMaxDomainSize2 = surfaceMaxDomainSize accuracy surfaceFunctions2 uvBounds2
  Range.width subdomain <= curveMaxDomainSize
    && Bounds2d.diameter uvBounds1 <= surfaceMaxDomainSize1
    && Bounds2d.diameter uvBounds2 <= surfaceMaxDomainSize2

surfaceMaxDomainSize :: Qty units -> SurfaceFunctions (space @ units) -> UvBounds -> Float
surfaceMaxDomainSize accuracy SurfaceFunctions{fuu, fuv, fvv} uvBounds = do
  let fuuBounds = VectorSurfaceFunction3d.evaluateBounds fuu uvBounds
  let fuvBounds = VectorSurfaceFunction3d.evaluateBounds fuv uvBounds
  let fvvBounds = VectorSurfaceFunction3d.evaluateBounds fvv uvBounds
  let fuuMagnitude = VectorBounds3d.magnitude fuuBounds
  let fuvMagnitude = VectorBounds3d.magnitude fuvBounds
  let fvvMagnitude = VectorBounds3d.magnitude fvvBounds
  SurfaceLinearization.maxDomainSize accuracy fuuMagnitude fuvMagnitude fvvMagnitude

boundarySurfaceMesh ::
  Qty units ->
  BoundarySurface (space @ units) ->
  Mesh (Point3d (space @ units))
boundarySurfaceMesh accuracy boundarySurface = do
  let BoundarySurface{surfaceFunctions, edgeLoops} = boundarySurface
  let boundaryPolygons = NonEmpty.map (toPolygon accuracy) edgeLoops
  let boundaryEdges = NonEmpty.collect Polygon2d.edges boundaryPolygons
  let edgeSet = Set2d.fromNonEmpty boundaryEdges
  let steinerPoints =
        generateSteinerPoints accuracy (Set2d.bounds edgeSet) edgeSet surfaceFunctions []
  let boundaryVertexLoops = NonEmpty.map Polygon2d.vertices boundaryPolygons
  let vertexMesh = CDT.unsafe boundaryVertexLoops steinerPoints Nothing
  Mesh.map vertexPosition vertexMesh

vertexPosition :: Vertex (space @ units) -> Point3d (space @ units)
vertexPosition (Vertex _ point) = point

toPolygon :: Qty units -> NonEmpty (Edge (space @ units)) -> Polygon2d (Vertex (space @ units))
toPolygon accuracy edgeLoop = Polygon2d (NonEmpty.collect (edgeVertices accuracy) edgeLoop)

generateSteinerPoints ::
  Qty units ->
  UvBounds ->
  Set2d (LineSegment2d (Vertex (space @ units))) UvCoordinates ->
  SurfaceFunctions (space @ units) ->
  List (Vertex (space @ units)) ->
  List (Vertex (space @ units))
generateSteinerPoints accuracy uvBounds edgeSet surfaceFunctions accumulated = do
  let Bounds2d uRange vRange = uvBounds
  let recurse = do
        let (u1, u2) = Range.bisect uRange
        let (v1, v2) = Range.bisect vRange
        let bounds11 = Bounds2d u1 v1
        let bounds12 = Bounds2d u1 v2
        let bounds21 = Bounds2d u2 v1
        let bounds22 = Bounds2d u2 v2
        accumulated
          |> generateSteinerPoints accuracy bounds11 edgeSet surfaceFunctions
          |> generateSteinerPoints accuracy bounds12 edgeSet surfaceFunctions
          |> generateSteinerPoints accuracy bounds21 edgeSet surfaceFunctions
          |> generateSteinerPoints accuracy bounds22 edgeSet surfaceFunctions
  let SurfaceFunctions{f} = surfaceFunctions
  let uvPoint = Point2d (Range.midpoint uRange) (Range.midpoint vRange)
  let return = Vertex uvPoint (SurfaceFunction3d.evaluate f uvPoint) : accumulated
  let maxDomainSize = surfaceMaxDomainSize accuracy surfaceFunctions uvBounds
  let accurate = Bounds2d.diameter uvBounds <= maxDomainSize
  case includeSubdomain uvBounds edgeSet of
    Resolved False -> accumulated
    Resolved True -> if accurate then return else recurse
    Unresolved -> recurse

includeSubdomain ::
  UvBounds ->
  Set2d (LineSegment2d (Vertex (space @ units))) UvCoordinates ->
  Fuzzy Bool
includeSubdomain subdomain edgeSet = Tolerance.exactly $
  case edgeSet of
    Set2d.Node nodeBounds leftChild rightChild
      | not (subdomain ^ nodeBounds) -> Resolved True
      | smallerThan nodeBounds subdomain -> Fuzzy.do
          includeLeft <- includeSubdomain subdomain leftChild
          includeRight <- includeSubdomain subdomain rightChild
          Resolved (includeLeft && includeRight)
      | otherwise -> Unresolved
    Set2d.Leaf edgeBounds _
      | not (subdomain ^ edgeBounds) -> Resolved True
      | smallerThan edgeBounds subdomain -> Resolved False
      | otherwise -> Unresolved

smallerThan :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Bool
smallerThan bounds1 bounds2 = Bounds2d.diameter bounds2 <= Bounds2d.diameter bounds1

toMesh :: Tolerance units => Qty units -> Body3d (space @ units) -> Mesh (Point3d (space @ units))
toMesh accuracy (Body3d boundarySurfaces) =
  Mesh.collect (boundarySurfaceMesh accuracy) boundarySurfaces
