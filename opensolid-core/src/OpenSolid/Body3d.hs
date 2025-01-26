{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

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
import OpenSolid.Float qualified as Float
import OpenSolid.LineSegment2d (LineSegment2d)
import OpenSolid.LineSegment2d qualified as LineSegment2d
import OpenSolid.Linearization qualified as Linearization
import OpenSolid.List qualified as List
import OpenSolid.Map (Map)
import OpenSolid.Map qualified as Map
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Mesh (Mesh)
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Polygon2d (Polygon2d (Polygon2d))
import OpenSolid.Polygon2d qualified as Polygon2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range (Range))
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
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.VectorBounds3d qualified as VectorBounds3d
import OpenSolid.VectorCurve3d (VectorCurve3d)
import OpenSolid.VectorCurve3d qualified as VectorCurve3d
import OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)
import OpenSolid.VectorSurfaceFunction3d qualified as VectorSurfaceFunction3d
import OpenSolid.Vertex2d (Vertex2d)
import OpenSolid.Vertex2d qualified as Vertex2d
import OpenSolid.Vertex3d (Vertex3d)
import OpenSolid.Vertex3d qualified as Vertex3d

type role Body3d nominal

newtype Body3d (coordinateSystem :: CoordinateSystem)
  = Body3d (NonEmpty (BoundarySurface coordinateSystem))

data BoundarySurface (coordinateSystem :: CoordinateSystem) where
  BoundarySurface ::
    { surfaceId :: SurfaceId
    , surfaceFunctions :: SurfaceFunctions (space @ units)
    , uvBounds :: UvBounds
    , edgeLoops :: NonEmpty (NonEmpty (Edge (space @ units)))
    } ->
    BoundarySurface (space @ units)

newtype SurfaceId = SurfaceId Int deriving (Eq, Ord, Show)

newtype LoopId = LoopId Int deriving (Eq, Ord, Show)

newtype HalfEdgeId = HalfEdgeId Int deriving (Eq, Ord, Show)

data EdgeId = EdgeId
  { surfaceId :: SurfaceId
  , loopId :: LoopId
  , halfEdgeId :: HalfEdgeId
  }
  deriving (Eq, Ord, Show)

data Edge (coordinateSystem :: CoordinateSystem) where
  Edge ::
    { startPoint :: Point3d (space @ units)
    , halfEdge :: HalfEdge (space @ units)
    , matingEdge :: HalfEdge (space @ units)
    } ->
    Edge (space @ units)
  DegenerateEdge ::
    { edgeId :: EdgeId
    , point :: Point3d (space @ units)
    , uvCurve :: Curve2d UvCoordinates
    } ->
    Edge (space @ units)

data HalfEdge (coordinateSystem :: CoordinateSystem) where
  HalfEdge ::
    { edgeId :: EdgeId
    , uvCurve :: Curve2d UvCoordinates -- UV curve parameterized by 3D arc length
    , curve3d :: Curve3d (space @ units) -- Arc length parameterized 3D curve
    , length :: Qty units -- Arc length of 3D curve
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

----- CONSTRUCTION -----

data SurfaceWithHalfEdges (coordinateSystem :: CoordinateSystem) where
  SurfaceWithHalfEdges ::
    { surfaceId :: SurfaceId
    , surfaceFunctions :: SurfaceFunctions (space @ units)
    , uvBounds :: UvBounds
    , halfEdgeLoops :: NonEmpty (NonEmpty (HalfEdge (space @ units)))
    } ->
    SurfaceWithHalfEdges (space @ units)

data Corner (coordinateSystem :: CoordinateSystem) where
  Corner :: {surfaceId :: SurfaceId, point :: Point3d (space @ units)} -> Corner (space @ units)

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

toSurfaceWithHalfEdges ::
  Tolerance units =>
  (Int, Surface3d (space @ units)) ->
  Result BoundedBy.Error (SurfaceWithHalfEdges (space @ units))
toSurfaceWithHalfEdges (surfaceIndex, surface) = do
  let surfaceFunctions = toSurfaceFunctions (Surface3d.function surface)
  let surfaceDomain = Surface3d.domain surface
  let loops = Region2d.outerLoop surfaceDomain :| Region2d.innerLoops surfaceDomain
  let surfaceId = SurfaceId surfaceIndex
  Result.map (SurfaceWithHalfEdges surfaceId surfaceFunctions (Region2d.bounds surfaceDomain)) $
    Result.collect (loopHalfEdges surfaceId surfaceFunctions) (NonEmpty.indexed loops)

toSurfaceFunctions :: SurfaceFunction3d (space @ units) -> SurfaceFunctions (space @ units)
toSurfaceFunctions f = do
  let fu = SurfaceFunction3d.derivative U f
  let fv = SurfaceFunction3d.derivative V f
  let fuu = VectorSurfaceFunction3d.derivative U fu
  let fuv = VectorSurfaceFunction3d.derivative V fu
  let fvv = VectorSurfaceFunction3d.derivative V fv
  SurfaceFunctions{f, fuu, fuv, fvv}

loopHalfEdges ::
  Tolerance units =>
  SurfaceId ->
  SurfaceFunctions (space @ units) ->
  (Int, NonEmpty (Curve2d UvCoordinates)) ->
  Result BoundedBy.Error (NonEmpty (HalfEdge (space @ units)))
loopHalfEdges surfaceId surfaceFunctions (loopId, loop) = do
  Result.collect (toHalfEdge surfaceId (LoopId loopId) surfaceFunctions) (NonEmpty.indexed loop)

toHalfEdge ::
  Tolerance units =>
  SurfaceId ->
  LoopId ->
  SurfaceFunctions (space @ units) ->
  (Int, Curve2d UvCoordinates) ->
  Result BoundedBy.Error (HalfEdge (space @ units))
toHalfEdge surfaceId loopId surfaceFunctions (halfEdgeIndex, uvCurve) = do
  let SurfaceFunctions{f} = surfaceFunctions
  let curve3d = f . uvCurve
  let halfEdgeId = HalfEdgeId halfEdgeIndex
  case Curve3d.arcLengthParameterization curve3d of
    Success (parameterization, length) ->
      Success $
        HalfEdge
          EdgeId{surfaceId, loopId, halfEdgeId}
          (uvCurve . parameterization)
          (curve3d . parameterization)
          length
          (Curve3d.bounds curve3d)
    Failure Curve3d.HasDegeneracy ->
      Failure BoundedBy.BoundaryCurveHasDegeneracy

getAllHalfEdges :: SurfaceWithHalfEdges (space @ units) -> NonEmpty (HalfEdge (space @ units))
getAllHalfEdges SurfaceWithHalfEdges{halfEdgeLoops} = NonEmpty.concat halfEdgeLoops

halfEdgeStartPoint :: HalfEdge (space @ units) -> Corner (space @ units)
halfEdgeStartPoint HalfEdge{edgeId = EdgeId{surfaceId}, curve3d} =
  Corner surfaceId (Curve3d.startPoint curve3d)

toBoundarySurface ::
  Tolerance units =>
  Set3d (Corner (space @ units)) (space @ units) ->
  Set3d (HalfEdge (space @ units)) (space @ units) ->
  SurfaceWithHalfEdges (space @ units) ->
  Result BoundedBy.Error (BoundarySurface (space @ units))
toBoundarySurface cornerSet halfEdgeSet surfaceWithHalfEdges = Result.do
  let SurfaceWithHalfEdges{surfaceId, surfaceFunctions, uvBounds, halfEdgeLoops} =
        surfaceWithHalfEdges
  edgeLoops <- Result.collect (Result.collect (toEdge cornerSet halfEdgeSet)) halfEdgeLoops
  Success BoundarySurface{surfaceId, surfaceFunctions, uvBounds, edgeLoops}

toEdge ::
  Tolerance units =>
  Set3d (Corner (space @ units)) (space @ units) ->
  Set3d (HalfEdge (space @ units)) (space @ units) ->
  HalfEdge (space @ units) ->
  Result BoundedBy.Error (Edge (space @ units))
toEdge cornerSet halfEdgeSet halfEdge = Result.do
  let HalfEdge{edgeId, curve3d, uvCurve, length} = halfEdge
  startPoint <- getCornerPoint (Curve3d.startPoint curve3d) cornerSet
  if length == Qty.zero
    then Success DegenerateEdge{edgeId, point = startPoint, uvCurve}
    else Result.do
      let matingEdgeCandidates = Set3d.filter (Bounds3d.bounds halfEdge) halfEdgeSet
      case List.filter (isMatingEdge halfEdge) matingEdgeCandidates of
        [] -> Failure BoundedBy.BoundaryHasGaps
        List.One matingEdge -> Success (Edge startPoint halfEdge matingEdge)
        List.TwoOrMore -> Failure BoundedBy.BoundaryIntersectsItself

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

isMatingEdge :: Tolerance units => HalfEdge (space @ units) -> HalfEdge (space @ units) -> Bool
isMatingEdge HalfEdge{curve3d = curve1} HalfEdge{curve3d = curve2} = do
  let pointsMatchAt t1 = Curve3d.evaluate curve1 t1 ~= Curve3d.evaluate curve2 (1.0 - t1)
  List.allSatisfy pointsMatchAt Parameter.samples

----- MESHING -----

data Vertex (coordinateSystem :: CoordinateSystem) where
  Vertex :: UvPoint -> Point3d (space @ units) -> Vertex (space @ units)

instance Vertex2d (Vertex (space @ units)) UvCoordinates where
  position (Vertex uvPoint _) = uvPoint

instance Vertex3d (Vertex (space @ units)) (space @ units) where
  position (Vertex _ point) = point

instance Bounded2d (Vertex (space @ units)) UvCoordinates where
  bounds (Vertex uvPoint _) = Bounds2d.constant uvPoint

toMesh :: Tolerance units => Qty units -> Body3d (space @ units) -> Mesh (Point3d (space @ units))
toMesh accuracy (Body3d boundarySurfaces) = do
  let boundarySurfaceList = NonEmpty.toList boundarySurfaces
  let surfaceSegmentsById = Map.fromList (boundarySurfaceSegments accuracy) boundarySurfaceList
  let innerEdgeVerticesById =
        NonEmpty.foldr (addBoundaryVertices accuracy surfaceSegmentsById) Map.empty boundarySurfaces
  Mesh.collect (boundarySurfaceMesh surfaceSegmentsById innerEdgeVerticesById) boundarySurfaces

boundarySurfaceSegments ::
  Qty units ->
  BoundarySurface (space @ units) ->
  (SurfaceId, Set2d UvBounds UvCoordinates)
boundarySurfaceSegments accuracy BoundarySurface{surfaceId, surfaceFunctions, uvBounds} =
  (surfaceId, boundarySurfaceSegmentSet accuracy surfaceFunctions uvBounds)

boundarySurfaceSegmentSet ::
  Qty units ->
  SurfaceFunctions (space @ units) ->
  UvBounds ->
  Set2d UvBounds UvCoordinates
boundarySurfaceSegmentSet accuracy surfaceFunctions uvBounds =
  if surfaceError surfaceFunctions uvBounds <= accuracy
    then Set2d.Leaf uvBounds uvBounds
    else do
      let Bounds2d uRange vRange = uvBounds
      let (u1, u2) = Range.bisect uRange
      let (v1, v2) = Range.bisect vRange
      let set11 = boundarySurfaceSegmentSet accuracy surfaceFunctions (Bounds2d u1 v1)
      let set12 = boundarySurfaceSegmentSet accuracy surfaceFunctions (Bounds2d u1 v2)
      let set21 = boundarySurfaceSegmentSet accuracy surfaceFunctions (Bounds2d u2 v1)
      let set22 = boundarySurfaceSegmentSet accuracy surfaceFunctions (Bounds2d u2 v2)
      let set1 = Set2d.Node (Bounds2d u1 vRange) set11 set12
      let set2 = Set2d.Node (Bounds2d u2 vRange) set21 set22
      Set2d.Node uvBounds set1 set2

surfaceError :: SurfaceFunctions (space @ units) -> UvBounds -> Qty units
surfaceError SurfaceFunctions{fuu, fuv, fvv} uvBounds = do
  let fuuBounds = VectorSurfaceFunction3d.evaluateBounds fuu uvBounds
  let fuvBounds = VectorSurfaceFunction3d.evaluateBounds fuv uvBounds
  let fvvBounds = VectorSurfaceFunction3d.evaluateBounds fvv uvBounds
  SurfaceLinearization.error fuuBounds fuvBounds fvvBounds uvBounds

addBoundaryVertices ::
  Qty units ->
  Map SurfaceId (Set2d UvBounds UvCoordinates) ->
  BoundarySurface (space @ units) ->
  Map EdgeId (List (Vertex (space @ units))) ->
  Map EdgeId (List (Vertex (space @ units)))
addBoundaryVertices accuracy surfaceSegmentsById boundarySurface accumulated = do
  let BoundarySurface{edgeLoops} = boundarySurface
  NonEmpty.foldr (addLoopVertices accuracy surfaceSegmentsById) accumulated edgeLoops

addLoopVertices ::
  Qty units ->
  Map SurfaceId (Set2d UvBounds UvCoordinates) ->
  NonEmpty (Edge (space @ units)) ->
  Map EdgeId (List (Vertex (space @ units))) ->
  Map EdgeId (List (Vertex (space @ units)))
addLoopVertices accuracy surfaceSegmentsById loop accumulated =
  NonEmpty.foldr (addEdgeVertices accuracy surfaceSegmentsById) accumulated loop

addEdgeVertices ::
  Qty units ->
  Map SurfaceId (Set2d UvBounds UvCoordinates) ->
  Edge (space @ units) ->
  Map EdgeId (List (Vertex (space @ units))) ->
  Map EdgeId (List (Vertex (space @ units)))
addEdgeVertices accuracy surfaceSegmentsById edge accumulated = do
  case edge of
    DegenerateEdge{edgeId, uvCurve, point} -> do
      let EdgeId{surfaceId} = edgeId
      case Map.get surfaceId surfaceSegmentsById of
        Just surfaceSegments -> do
          let edgePredicate = degenerateEdgeLinearizationPredicate uvCurve surfaceSegments
          let tValues = Domain1d.innerSamplingPoints edgePredicate
          let vertex tValue = Vertex (Curve2d.evaluate uvCurve tValue) point
          Map.set edgeId (List.map vertex tValues) accumulated
        Nothing ->
          internalError "Should always be able to look up surface segments for a given edge"
    Edge{halfEdge, matingEdge} -> do
      let HalfEdge{edgeId, uvCurve, curve3d} = halfEdge
      let HalfEdge{edgeId = matingEdgeId, uvCurve = matingUvCurve} = matingEdge
      if edgeId < matingEdgeId
        then do
          let EdgeId{surfaceId} = edgeId
          let EdgeId{surfaceId = matingSurfaceId} = matingEdgeId
          case (Map.get surfaceId surfaceSegmentsById, Map.get matingSurfaceId surfaceSegmentsById) of
            (Just surfaceSegments, Just matingSurfaceSegments) -> do
              let edgePredicate =
                    edgeLinearizationPredicate
                      accuracy
                      halfEdge
                      matingEdge
                      surfaceSegments
                      matingSurfaceSegments
                      (VectorCurve3d.derivative (Curve3d.derivative curve3d))
              let tValues = Domain1d.innerSamplingPoints edgePredicate
              let vertexPair tValue = do
                    let point = Curve3d.evaluate curve3d tValue
                    let uvPoint = Curve2d.evaluate uvCurve tValue
                    let matingUvPoint = Curve2d.evaluate matingUvCurve (1.0 - tValue)
                    (Vertex uvPoint point, Vertex matingUvPoint point)
              let (vertices, matingVertices) = List.unzip2 (List.map vertexPair tValues)
              accumulated
                |> Map.set edgeId vertices
                |> Map.set matingEdgeId (List.reverse matingVertices)
            _ -> internalError "Should always be able to look up surface segments for a given edge"
        else accumulated

edgeLinearizationPredicate ::
  Qty units ->
  HalfEdge (space @ units) ->
  HalfEdge (space @ units) ->
  Set2d UvBounds UvCoordinates ->
  Set2d UvBounds UvCoordinates ->
  VectorCurve3d (space @ units) ->
  Range Unitless ->
  Bool
edgeLinearizationPredicate
  accuracy
  halfEdge
  matingEdge
  surfaceSegments
  matingSurfaceSegments
  edgeSecondDerivative
  tRange = do
    let HalfEdge{uvCurve} = halfEdge
    let HalfEdge{uvCurve = matingUvCurve} = matingEdge
    let Range tStart tEnd = tRange
    let uvStart = Curve2d.evaluate uvCurve tStart
    let uvEnd = Curve2d.evaluate uvCurve tEnd
    let matingUvStart = Curve2d.evaluate matingUvCurve (1.0 - tStart)
    let matingUvEnd = Curve2d.evaluate matingUvCurve (1.0 - tEnd)
    let uvBounds = Bounds2d.hull2 uvStart uvEnd
    let matingUvBounds = Bounds2d.hull2 matingUvStart matingUvEnd
    let edgeSize = Point2d.distanceFrom uvStart uvEnd
    let matingEdgeSize = Point2d.distanceFrom matingUvStart matingUvEnd
    let edgeSecondDerivativeBounds = VectorCurve3d.evaluateBounds edgeSecondDerivative tRange
    let edgeSecondDerivativeMagnitude = VectorBounds3d.magnitude edgeSecondDerivativeBounds
    Linearization.error edgeSecondDerivativeMagnitude tRange <= accuracy
      && validEdge uvBounds edgeSize surfaceSegments
      && validEdge matingUvBounds matingEdgeSize matingSurfaceSegments

degenerateEdgeLinearizationPredicate ::
  Curve2d UvCoordinates ->
  Set2d UvBounds UvCoordinates ->
  Range Unitless ->
  Bool
degenerateEdgeLinearizationPredicate uvCurve surfaceSegments tRange = do
  let Range tStart tEnd = tRange
  let uvStart = Curve2d.evaluate uvCurve tStart
  let uvEnd = Curve2d.evaluate uvCurve tEnd
  let uvBounds = Bounds2d.hull2 uvStart uvEnd
  let edgeSize = Point2d.distanceFrom uvStart uvEnd
  validEdge uvBounds edgeSize surfaceSegments

validEdge :: UvBounds -> Float -> Set2d UvBounds UvCoordinates -> Bool
validEdge edgeBounds edgeLength surfaceSegments = Tolerance.exactly
  case surfaceSegments of
    Set2d.Node nodeBounds left right ->
      not (edgeBounds ^ nodeBounds)
        || (validEdge edgeBounds edgeLength left && validEdge edgeBounds edgeLength right)
    Set2d.Leaf leafBounds _ ->
      not (edgeBounds ^ leafBounds)
        || edgeLength <= Float.sqrt 2.0 * Bounds2d.diameter leafBounds

boundarySurfaceMesh ::
  Map SurfaceId (Set2d UvBounds UvCoordinates) ->
  Map EdgeId (List (Vertex (space @ units))) ->
  BoundarySurface (space @ units) ->
  Mesh (Point3d (space @ units))
boundarySurfaceMesh surfaceSegmentsById innerEdgeVerticesById boundarySurface = do
  let BoundarySurface{surfaceId, surfaceFunctions, edgeLoops} = boundarySurface
  let boundaryPolygons = NonEmpty.map (toPolygon innerEdgeVerticesById) edgeLoops
  let boundarySegments = NonEmpty.collect Polygon2d.edges boundaryPolygons
  let boundarySegmentSet = Set2d.fromNonEmpty boundarySegments
  case Map.get surfaceId surfaceSegmentsById of
    Nothing -> internalError "Should always be able to look up surface segments by ID"
    Just surfaceSegments -> do
      let steinerPoints =
            case surfaceSegments of
              Set2d.Leaf{} ->
                -- Special case, if the surface as a whole is sufficiently linear
                -- (only needs a single segment to approximate it)
                -- then we don't need any interior points at all
                -- (sufficient to just use the boundary points)
                []
              Set2d.Node{} ->
                Maybe.collect (steinerPoint boundarySegmentSet) (Set2d.toList surfaceSegments)
      let SurfaceFunctions{f} = surfaceFunctions
      let steinerVertex uvPoint = Vertex uvPoint (SurfaceFunction3d.evaluate f uvPoint)
      let steinerVertices = List.map steinerVertex steinerPoints
      let boundaryVertexLoops = NonEmpty.map Polygon2d.vertices boundaryPolygons
      let vertexMesh = CDT.unsafe boundaryVertexLoops steinerVertices Nothing
      Mesh.map Vertex3d.position vertexMesh

toPolygon ::
  Map EdgeId (List (Vertex (space @ units))) ->
  NonEmpty (Edge (space @ units)) ->
  Polygon2d (Vertex (space @ units))
toPolygon innerEdgeVerticesById loop =
  Polygon2d (NonEmpty.collect (edgeVertices innerEdgeVerticesById) loop)

edgeVertices ::
  Map EdgeId (List (Vertex (space @ units))) ->
  Edge (space @ units) ->
  NonEmpty (Vertex (space @ units))
edgeVertices innerEdgeVerticesById edge = case edge of
  DegenerateEdge{edgeId, uvCurve, point} ->
    edgeVerticesImpl innerEdgeVerticesById edgeId point uvCurve
  Edge{startPoint, halfEdge = HalfEdge{edgeId, uvCurve}} ->
    edgeVerticesImpl innerEdgeVerticesById edgeId startPoint uvCurve

edgeVerticesImpl ::
  Map EdgeId (List (Vertex (space @ units))) ->
  EdgeId ->
  Point3d (space @ units) ->
  Curve2d UvCoordinates ->
  NonEmpty (Vertex (space @ units))
edgeVerticesImpl innerEdgeVerticesById edgeId startPoint uvCurve =
  case Map.get edgeId innerEdgeVerticesById of
    Just innerEdgeVertices -> Vertex (Curve2d.startPoint uvCurve) startPoint :| innerEdgeVertices
    Nothing -> internalError "Should always be able to look up internal edge vertices by ID"

steinerPoint ::
  Set2d (LineSegment2d (Vertex (space @ units))) UvCoordinates ->
  UvBounds ->
  Maybe UvPoint
steinerPoint boundarySegmentSet uvBounds = do
  let Bounds2d uRange vRange = uvBounds
  let uvPoint = Point2d (Range.midpoint uRange) (Range.midpoint vRange)
  if isValidSteinerPoint boundarySegmentSet uvPoint then Just uvPoint else Nothing

isValidSteinerPoint ::
  Set2d (LineSegment2d (Vertex (space @ units))) UvCoordinates ->
  UvPoint ->
  Bool
isValidSteinerPoint edgeSet uvPoint = case edgeSet of
  Set2d.Node nodeBounds left right -> do
    let distance = VectorBounds2d.magnitude (uvPoint - nodeBounds)
    Range.lowerBound distance >= 0.5 * Bounds2d.diameter nodeBounds
      || (isValidSteinerPoint left uvPoint && isValidSteinerPoint right uvPoint)
  Set2d.Leaf _ edge -> LineSegment2d.distanceTo uvPoint edge >= 0.5 * LineSegment2d.length edge
