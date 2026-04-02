module OpenSolid.Body3D
  ( Body3D
  , block
  , sphere
  , cylinder
  , cylinderAlong
  , extruded
  , translational
  , revolved
  , boundedBy
  , toPointMesh
  , toSurfaceMesh
  , surfaces
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Array qualified as Array
import OpenSolid.Axis2D (Axis2D)
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Axis3D (Axis3D (Axis3D))
import OpenSolid.Axis3D qualified as Axis3D
import OpenSolid.Body3D.BoundedBy qualified as BoundedBy
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.CDT qualified as CDT
import OpenSolid.Circle2D qualified as Circle2D
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Curve3D (Curve3D)
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.Domain1D qualified as Domain1D
import OpenSolid.Error (IsDegenerate (IsDegenerate))
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.InternalError (InternalError (InternalError))
import OpenSolid.InternalError qualified as InternalError
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Length (Length)
import OpenSolid.Line2D (Line2D)
import OpenSolid.Line2D qualified as Line2D
import OpenSolid.List qualified as List
import OpenSolid.Map (Map)
import OpenSolid.Map qualified as Map
import OpenSolid.Mesh (Mesh)
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Number qualified as Number
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Plane3D (Plane3D (Plane3D))
import OpenSolid.Plane3D qualified as Plane3D
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Polygon2D (Polygon2D (Polygon2D))
import OpenSolid.Polygon2D qualified as Polygon2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Region2D (Region2D)
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Resolution (Resolution)
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Set2D (Set2D)
import OpenSolid.Set2D qualified as Set2D
import OpenSolid.Set3D (Set3D)
import OpenSolid.Set3D qualified as Set3D
import OpenSolid.Surface3D (Surface3D)
import OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.Surface3D.Revolved qualified as Surface3D.Revolved
import OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.SurfaceFunction3D.Nondegenerate qualified as SurfaceFunction3D.Nondegenerate
import OpenSolid.SurfaceVertex3D (SurfaceVertex3D (SurfaceVertex3D, normal, position))
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Unboxed.Math
import OpenSolid.UvBounds (UvBounds, pattern UvBounds)
import OpenSolid.UvPoint (UvPoint, pattern UvPoint)
import OpenSolid.Vector qualified as Vector
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorCurve3D (VectorCurve3D)
import OpenSolid.VectorCurve3D qualified as VectorCurve3D
import OpenSolid.World3D qualified as World3D

-- | A solid body in 3D, defined by a set of boundary surfaces.
newtype Body3D space
  = Body3D (NonEmpty (BoundarySurface space))

instance FFI (Body3D FFI.Space) where
  representation = FFI.classRepresentation "Body3D"

data BoundarySurface space = BoundarySurface
  { surfaceId :: SurfaceId
  , orientedSurface :: Surface3D space
  , surfaceFunction :: SurfaceFunction3D space
  , handedness :: Sign
  , uvBounds :: UvBounds
  , edgeLoops :: NonEmpty (NonEmpty (Edge space))
  }

newtype SurfaceId = SurfaceId Int deriving (Eq, Ord, Show)

data Edge space where
  PrimaryEdge ::
    { halfEdgeId :: HalfEdgeId
    , uvCurve :: Curve2D Unitless
    , curve3D :: Curve3D space
    , matingId :: HalfEdgeId
    , matingUvCurve :: Curve2D Unitless
    , correctlyAligned :: Bool
    } ->
    Edge space
  SecondaryEdge ::
    { halfEdgeId :: HalfEdgeId
    , uvStartPoint :: UvPoint
    } ->
    Edge space
  DegenerateEdge ::
    { halfEdgeId :: HalfEdgeId
    , uvCurve :: Curve2D Unitless
    } ->
    Edge space

data HalfEdgeId = HalfEdgeId
  { surfaceId :: SurfaceId
  , loopId :: LoopId
  , curveId :: CurveId
  }
  deriving (Eq, Ord, Show)

newtype LoopId = LoopId Int deriving (Eq, Ord, Show)

newtype CurveId = CurveId Int deriving (Eq, Ord, Show)

----- CONSTRUCTION -----

data SurfaceWithHalfEdges space = SurfaceWithHalfEdges
  { surfaceId :: SurfaceId
  , surface :: Surface3D space
  , handedness :: Sign
  , halfEdgeLoops :: NonEmpty (NonEmpty (HalfEdge space))
  }

data HalfEdge space where
  HalfEdge ::
    { halfEdgeId :: HalfEdgeId
    , uvCurve :: Curve2D Unitless -- UV curve parameterized by 3D arc length
    , curve3D :: Curve3D space -- Arc length parameterized 3D curve
    , length :: Length -- Arc length of 3D curve
    , bounds :: Bounds3D space -- Bounds on 3D curve
    } ->
    HalfEdge space
  DegenerateHalfEdge ::
    { halfEdgeId :: HalfEdgeId
    , uvCurve :: Curve2D Unitless
    , point :: Point3D space
    } ->
    HalfEdge space

halfEdgeBounds :: HalfEdge space -> Bounds3D space
halfEdgeBounds HalfEdge{bounds} = bounds
halfEdgeBounds DegenerateHalfEdge{point} = Bounds3D.constant point

data MatingEdge = MatingEdge
  { halfEdgeId :: HalfEdgeId
  , uvCurve :: Curve2D Unitless
  , correctlyAligned :: Bool
  }

data SurfaceRegistry space = SurfaceRegistry
  { unprocessed :: Map SurfaceId (SurfaceWithHalfEdges space)
  , processed :: Map SurfaceId (SurfaceWithHalfEdges space)
  , edges :: Map HalfEdgeId (Edge space)
  }

data EmptyBody = EmptyBody deriving (Eq, Show)

{-| Create a rectangular block body.

Fails if the given bounds are empty (the length, width, or height is zero).
-}
block :: Tolerance Meters => Bounds3D space -> Result EmptyBody (Body3D space)
block bounds =
  case Region2D.rectangle (Bounds3D.projectInto World3D.topPlane bounds) of
    Error Region2D.EmptyRegion -> Error EmptyBody
    Ok profile -> do
      let Interval h1 h2 = Bounds3D.upwardCoordinate bounds
      if h1 ~= h2
        then Error EmptyBody
        else case extruded World3D.topPlane profile h1 h2 of
          Ok body -> Ok body
          Error _ ->
            InternalError.throw "Constructing block body from non-empty bounds should not fail"

{-| Create a sphere with the given center point and diameter.

Fails if the given diameter is zero.
-}
sphere ::
  Tolerance Meters =>
  "centerPoint" ::: Point3D space ->
  "diameter" ::: Length ->
  Result EmptyBody (Body3D space)
sphere ("centerPoint" ::: centerPoint) ("diameter" ::: diameter) =
  if diameter ~= Quantity.zero
    then Error EmptyBody
    else do
      let sketchPlane = Plane3D centerPoint World3D.frontPlane.orientation
      let radius = 0.5 * diameter
      let p1 = Point2D.y -radius
      let p2 = Point2D.y radius
      let profileCurves = [Curve2D.arcFrom p1 p2 Angle.pi, Curve2D.lineFrom p2 p1]
      case Region2D.boundedBy profileCurves of
        Error _ -> throw (InternalError "Semicircle profile construction should always succeed")
        Ok profile ->
          case revolved sketchPlane profile Axis2D.y Angle.twoPi of
            Ok body -> Ok body
            Error _ ->
              throw (InternalError "Constructing sphere from non-zero radius should not fail")

{-| Create a cylindrical body from a start point, end point and diameter.

Fails if the cylinder length or diameter is zero.
-}
cylinder ::
  Tolerance Meters =>
  Point3D space ->
  Point3D space ->
  "diameter" ::: Length ->
  Result EmptyBody (Body3D space)
cylinder startPoint endPoint ("diameter" ::: diameter) =
  case Vector3D.magnitudeAndDirection (endPoint - startPoint) of
    Error Vector.IsZero -> Error EmptyBody
    Ok (length, direction) ->
      cylinderAlong (Axis3D startPoint direction) Quantity.zero length (#diameter diameter)

{-| Create a cylindrical body along a given axis.

In addition to the axis itself, you will need to provide:

- Where along the axis the cylinder starts and ends
  (given as a range of distances along the axis).
- The cylinder diameter.

Failes if the cylinder length or diameter is zero.
-}
cylinderAlong ::
  Tolerance Meters =>
  Axis3D space ->
  Length ->
  Length ->
  "diameter" ::: Length ->
  Result EmptyBody (Body3D space)
cylinderAlong axis d1 d2 ("diameter" ::: diameter) =
  case Region2D.circle (Circle2D.withDiameter diameter Point2D.origin) of
    Error Region2D.EmptyRegion -> Error EmptyBody
    Ok profile ->
      if d1 ~= d2
        then Error EmptyBody
        else case extruded (Axis3D.normalPlane axis) profile d1 d2 of
          Ok body -> Ok body
          Error _ -> InternalError.throw "Constructing non-empty cylinder body should not fail"

-- | Create an extruded body from a sketch plane and profile.
extruded ::
  Tolerance Meters =>
  Plane3D space ->
  Region2D Meters ->
  Length ->
  Length ->
  Result BoundedBy.Error (Body3D space)
extruded sketchPlane profile d1 d2 = do
  let normal = Plane3D.normalDirection sketchPlane
  let v1 = d1 * normal
  let v2 = d2 * normal
  translational sketchPlane profile (VectorCurve3D.interpolateFrom v1 v2)

translational ::
  Tolerance Meters =>
  Plane3D space ->
  Region2D Meters ->
  VectorCurve3D Meters space ->
  Result BoundedBy.Error (Body3D space)
translational sketchPlane profile displacement = do
  let v0 = VectorCurve3D.startValue displacement
  let v1 = VectorCurve3D.endValue displacement
  let startPlane = Plane3D.translateBy v0 sketchPlane
  let endPlane = Plane3D.translateBy v1 sketchPlane
  let startCap = Surface3D.on startPlane profile
  let endCap = Surface3D.on endPlane profile
  let sideSurface curve = Surface3D.translational (Curve2D.placeOn sketchPlane curve) displacement
  let sideSurfaces = List.map sideSurface (NonEmpty.toList (Region2D.boundaryCurves profile))
  let initialDerivative = VectorCurve3D.derivativeValue displacement 0.0
  case Quantity.sign (initialDerivative `dot` Plane3D.normalDirection sketchPlane) of
    Positive -> boundedBy (endCap : startCap : sideSurfaces)
    Negative -> boundedBy (startCap : endCap : sideSurfaces)

{-| Create a revolved body from a sketch plane and profile.

Note that the revolution profile and revolution axis
are both defined within the given sketch plane.

A positive angle will result in a counterclockwise revolution around the axis,
and a negative angle will result in a clockwise revolution.
-}
revolved ::
  Tolerance Meters =>
  Plane3D space ->
  Region2D Meters ->
  Axis2D Meters ->
  Angle ->
  Result BoundedBy.Error (Body3D space)
revolved startPlane profile axis2D angle = do
  let axis3D = Axis2D.placeOn startPlane axis2D
  let profileCurves = Region2D.boundaryCurves profile
  let offAxisCurves = NonEmpty.filter (not . Curve2D.isOnAxis axis2D) profileCurves
  let signedDistanceCurves = List.map (Curve2D.distanceRightOf axis2D) offAxisCurves
  profileSign <-
    case Result.collect Curve1D.sign signedDistanceCurves of
      Error Curve1D.CrossesZero -> Error BoundedBy.BoundaryIntersectsItself
      Ok curveSigns
        | List.all (== Positive) curveSigns -> Ok Positive
        | List.all (== Negative) curveSigns -> Ok Negative
        | otherwise -> Error BoundedBy.BoundaryIntersectsItself

  let endPlane = Plane3D.rotateAround axis3D angle startPlane
  let unflippedStartCap = Surface3D.on startPlane profile
  let unflippedEndCap = Surface3D.on endPlane profile
  let sideSurface profileCurve = Surface3D.revolved startPlane profileCurve axis2D angle
  sideSurfaces <-
    case Result.collect sideSurface offAxisCurves of
      Error Surface3D.Revolved.ProfileIsOnAxis ->
        throw (InternalError "Should have already filtered out all on-axis curves")
      Error Surface3D.Revolved.ProfileCrossesAxis ->
        throw (InternalError "Should have already failed computing profileSign if any profile curve crosses the revolution axis")
      Ok surface -> Ok surface
  let startBoundaryCurves = Surface3D.boundaryCurves unflippedStartCap
  let endBoundaryCurves = Surface3D.boundaryCurves unflippedEndCap
  let isFullRevolution = startBoundaryCurves ~= endBoundaryCurves
  let endSurfaces =
        if isFullRevolution
          then []
          else case profileSign of
            Positive -> [unflippedStartCap, Surface3D.flip unflippedEndCap]
            Negative -> [Surface3D.flip unflippedStartCap, unflippedEndCap]
  boundedBy (endSurfaces <> sideSurfaces)

{-| Create a body bounded by the given surfaces.
The surfaces do not have to have consistent orientation,
but currently the *first* surface must have the correct orientation
since all others will be flipped if necessary to match it.
-}
boundedBy :: Tolerance Meters => List (Surface3D space) -> Result BoundedBy.Error (Body3D space)
boundedBy [] = Error BoundedBy.EmptyBody
boundedBy (NonEmpty givenSurfaces) = do
  let surfacesWithHalfEdges = NonEmpty.mapWithIndex toSurfaceWithHalfEdges givenSurfaces
  let firstSurfaceWithHalfEdges = NonEmpty.first surfacesWithHalfEdges
  let halfEdges = NonEmpty.combine getAllHalfEdges surfacesWithHalfEdges
  let halfEdgeSet = Set3D.partitionBy halfEdgeBounds halfEdges
  let initialSurfaceRegistry =
        SurfaceRegistry
          { unprocessed =
              Map.fromList $
                List.map surfaceWithHalfEdgesMapEntry $
                  NonEmpty.toList surfacesWithHalfEdges
          , processed = Map.empty
          , edges = Map.empty
          }
  finalSurfaceRegistry <-
    registerSurfaceWithHalfEdges
      halfEdgeSet
      initialSurfaceRegistry
      firstSurfaceWithHalfEdges
  let SurfaceRegistry{unprocessed, processed, edges} = finalSurfaceRegistry
  case (Map.values unprocessed, Map.values processed) of
    (NonEmpty _, _) -> Error BoundedBy.BoundaryHasGaps
    ([], []) -> InternalError.throw "Should always have at least one processed surface"
    ([], NonEmpty processedSurfaces) ->
      Ok (Body3D (NonEmpty.map (toBoundarySurface edges) processedSurfaces))

surfaceWithHalfEdgesMapEntry ::
  SurfaceWithHalfEdges space ->
  (SurfaceId, SurfaceWithHalfEdges space)
surfaceWithHalfEdgesMapEntry surfaceWithHalfEdges = do
  let SurfaceWithHalfEdges{surfaceId} = surfaceWithHalfEdges
  (surfaceId, surfaceWithHalfEdges)

toSurfaceWithHalfEdges :: Tolerance Meters => Int -> Surface3D space -> SurfaceWithHalfEdges space
toSurfaceWithHalfEdges surfaceIndex surface = do
  let loops = Region2D.boundaryLoops (Surface3D.domain surface)
  let surfaceId = SurfaceId surfaceIndex
  let halfEdges = NonEmpty.mapWithIndex (loopHalfEdges surfaceId (Surface3D.function surface)) loops
  SurfaceWithHalfEdges surfaceId surface Positive halfEdges

loopHalfEdges ::
  Tolerance Meters =>
  SurfaceId ->
  SurfaceFunction3D space ->
  Int ->
  NonEmpty (Curve2D Unitless) ->
  NonEmpty (HalfEdge space)
loopHalfEdges surfaceId surfaceFunction loopIndex loop =
  NonEmpty.mapWithIndex (toHalfEdge surfaceId (LoopId loopIndex) surfaceFunction) loop

toHalfEdge ::
  Tolerance Meters =>
  SurfaceId ->
  LoopId ->
  SurfaceFunction3D space ->
  Int ->
  Curve2D Unitless ->
  HalfEdge space
toHalfEdge surfaceId loopId surfaceFunction curveIndex uvCurve = do
  let curve3D = surfaceFunction . uvCurve
  let curveId = CurveId curveIndex
  let halfEdgeId = HalfEdgeId{surfaceId, loopId, curveId}
  let (parameterization, length) = Curve3D.arcLengthParameterization curve3D
  if length == Quantity.zero
    then DegenerateHalfEdge{halfEdgeId, uvCurve, point = Curve3D.startPoint curve3D}
    else
      HalfEdge
        { halfEdgeId
        , uvCurve = uvCurve . parameterization
        , curve3D = curve3D . parameterization
        , length
        , bounds = Curve3D.overallBounds curve3D
        }

getAllHalfEdges :: SurfaceWithHalfEdges space -> NonEmpty (HalfEdge space)
getAllHalfEdges SurfaceWithHalfEdges{halfEdgeLoops} = NonEmpty.concat halfEdgeLoops

registerSurfaceWithHalfEdges ::
  Tolerance Meters =>
  Set3D space (HalfEdge space) ->
  SurfaceRegistry space ->
  SurfaceWithHalfEdges space ->
  Result BoundedBy.Error (SurfaceRegistry space)
registerSurfaceWithHalfEdges halfEdgeSet surfaceRegistry surfaceWithHalfEdges = do
  let SurfaceWithHalfEdges{surfaceId, handedness} = surfaceWithHalfEdges
  let SurfaceRegistry{unprocessed, processed, edges} = surfaceRegistry
  let updatedRegistry =
        SurfaceRegistry
          { unprocessed = Map.remove surfaceId unprocessed
          , processed = Map.set surfaceId surfaceWithHalfEdges processed
          , edges
          }
  let halfEdges = NonEmpty.toList (getAllHalfEdges surfaceWithHalfEdges)
  Result.foldl (registerHalfEdge handedness halfEdgeSet) updatedRegistry halfEdges

registerHalfEdge ::
  Tolerance Meters =>
  Sign ->
  Set3D space (HalfEdge space) ->
  SurfaceRegistry space ->
  HalfEdge space ->
  Result BoundedBy.Error (SurfaceRegistry space)
registerHalfEdge parentHandedness halfEdgeSet surfaceRegistry halfEdge = do
  let SurfaceRegistry{unprocessed, processed, edges} = surfaceRegistry
  case halfEdge of
    DegenerateHalfEdge{halfEdgeId, uvCurve} -> do
      let edge = DegenerateEdge{halfEdgeId, uvCurve}
      Ok SurfaceRegistry{unprocessed, processed, edges = Map.set halfEdgeId edge edges}
    HalfEdge{halfEdgeId, uvCurve, curve3D} -> do
      let matingEdgeCandidates = Set3D.filter (halfEdgeBounds halfEdge) halfEdgeSet
      case List.filterMap (toMatingEdge halfEdgeId curve3D) matingEdgeCandidates of
        [] -> Error BoundedBy.BoundaryHasGaps
        List.TwoOrMore -> Error BoundedBy.BoundaryIntersectsItself
        List.One MatingEdge{halfEdgeId = matingId, uvCurve = matingUvCurve, correctlyAligned} -> do
          let HalfEdgeId{surfaceId = matingSurfaceId} = matingId
          let edge =
                if halfEdgeId < matingId
                  then
                    PrimaryEdge
                      { halfEdgeId
                      , uvCurve
                      , curve3D
                      , matingId
                      , matingUvCurve
                      , correctlyAligned
                      }
                  else SecondaryEdge{halfEdgeId, uvStartPoint = Curve2D.startPoint uvCurve}
          let updatedRegistry =
                SurfaceRegistry{unprocessed, processed, edges = Map.set halfEdgeId edge edges}
          let matingHandedness = if correctlyAligned then parentHandedness else -parentHandedness
          case (Map.get matingSurfaceId unprocessed, Map.get matingSurfaceId processed) of
            (Nothing, Nothing) -> throw (InternalError "No surface found for half-edge")
            (Just _, Just _) -> throw (InternalError "Multiple surfaces found for half-edge")
            (Nothing, Just SurfaceWithHalfEdges{handedness}) ->
              if handedness == matingHandedness
                then Ok updatedRegistry
                else Error BoundedBy.BoundaryIntersectsItself
            (Just unprocessedSurface, Nothing) -> do
              let processedSurface = setHandedness matingHandedness unprocessedSurface
              registerSurfaceWithHalfEdges halfEdgeSet updatedRegistry processedSurface

setHandedness :: Sign -> SurfaceWithHalfEdges space -> SurfaceWithHalfEdges space
setHandedness handedness SurfaceWithHalfEdges{surfaceId, surface, halfEdgeLoops} =
  SurfaceWithHalfEdges{handedness, surfaceId, surface, halfEdgeLoops}

toBoundarySurface ::
  Map HalfEdgeId (Edge space) ->
  SurfaceWithHalfEdges space ->
  BoundarySurface space
toBoundarySurface edges SurfaceWithHalfEdges{surfaceId, surface, handedness, halfEdgeLoops} =
  BoundarySurface
    { surfaceId
    , orientedSurface = case handedness of
        Positive -> surface
        Negative -> Surface3D.flip surface
    , surfaceFunction = Surface3D.function surface
    , handedness
    , uvBounds = Region2D.bounds (Surface3D.domain surface)
    , edgeLoops = NonEmpty.map (NonEmpty.map (toEdge edges)) halfEdgeLoops
    }

toEdge :: Map HalfEdgeId (Edge space) -> HalfEdge space -> Edge space
toEdge edges halfEdge =
  case Map.get (getHalfEdgeId halfEdge) edges of
    Just edge -> edge
    Nothing -> throw (InternalError "Should always be able to find edge for processed half-edge")

getHalfEdgeId :: HalfEdge space -> HalfEdgeId
getHalfEdgeId HalfEdge{halfEdgeId} = halfEdgeId
getHalfEdgeId DegenerateHalfEdge{halfEdgeId} = halfEdgeId

toMatingEdge ::
  Tolerance Meters =>
  HalfEdgeId ->
  Curve3D space ->
  HalfEdge space ->
  Maybe MatingEdge
toMatingEdge _ _ DegenerateHalfEdge{} = Nothing
toMatingEdge id1 curve1 HalfEdge{halfEdgeId = id2, curve3D = curve2, uvCurve}
  | id1 == id2 = Nothing
  | otherwise = do
      let points1 = NonEmpty.map (Curve3D.point curve1) Parameter.samples
      let points2 = NonEmpty.map (Curve3D.point curve2) Parameter.samples
      if
        | points1 ~= points2 ->
            Just MatingEdge{halfEdgeId = id2, uvCurve, correctlyAligned = False}
        | points1 ~= NonEmpty.reverse points2 ->
            Just MatingEdge{halfEdgeId = id2, uvCurve, correctlyAligned = True}
        | otherwise -> Nothing

----- MESHING -----

toPointMesh :: Tolerance Meters => Resolution Meters -> Body3D space -> Mesh (Point3D space)
toPointMesh resolution body = do
  let toVertex function _ uvPoint = SurfaceFunction3D.Nondegenerate.point function uvPoint
  toMesh resolution toVertex body

toSurfaceMesh ::
  Tolerance Meters =>
  Resolution Meters ->
  Body3D space ->
  Mesh (SurfaceVertex3D space)
toSurfaceMesh resolution body = do
  let toVertex function handedness uvPoint =
        SurfaceVertex3D
          { position = SurfaceFunction3D.Nondegenerate.point function uvPoint
          , normal =
              handedness * SurfaceFunction3D.Nondegenerate.normalDirectionValue function uvPoint
          }
  toMesh resolution toVertex body

type ToVertex vertex space =
  Nondegenerate (SurfaceFunction3D space) ->
  Sign ->
  UvPoint ->
  vertex

toMesh :: Tolerance Meters => Resolution Meters -> ToVertex vertex space -> Body3D space -> Mesh vertex
toMesh resolution toVertex (Body3D boundarySurfaces) = do
  let boundarySurfaceList = NonEmpty.toList boundarySurfaces
  let surfaceSegmentsById =
        Map.fromList (List.map (boundarySurfaceSegments resolution) boundarySurfaceList)
  let innerEdgeVerticesById =
        NonEmpty.foldr
          (addBoundaryInnerEdgeVertices resolution surfaceSegmentsById)
          Map.empty
          boundarySurfaces
  Mesh.combine
    (boundarySurfaceMesh surfaceSegmentsById innerEdgeVerticesById toVertex)
    boundarySurfaceList

boundarySurfaceSegments ::
  Resolution Meters ->
  BoundarySurface space ->
  (SurfaceId, Set2D Unitless UvBounds)
boundarySurfaceSegments resolution BoundarySurface{surfaceId, surfaceFunction, uvBounds} = do
  let Bounds2D (Interval u1 u2) (Interval v1 v2) = uvBounds
  let p11 = SurfaceFunction3D.point surfaceFunction (UvPoint u1 v1)
  let p21 = SurfaceFunction3D.point surfaceFunction (UvPoint u2 v1)
  let p12 = SurfaceFunction3D.point surfaceFunction (UvPoint u1 v2)
  let p22 = SurfaceFunction3D.point surfaceFunction (UvPoint u2 v2)
  (surfaceId, boundarySurfaceSegmentSet resolution surfaceFunction uvBounds p11 p21 p12 p22)

boundarySurfaceSegmentSet ::
  Resolution Meters ->
  SurfaceFunction3D space ->
  UvBounds ->
  Point3D space ->
  Point3D space ->
  Point3D space ->
  Point3D space ->
  Set2D Unitless UvBounds
boundarySurfaceSegmentSet resolution function uvBounds p11 p21 p12 p22 = do
  let d1 = p21 - p12
  let d2 = p22 - p11
  let size = max (Vector3D.magnitude d1) (Vector3D.magnitude d2)
  let n = Vector3D.normalize (d1 `cross_` d2)
  let UvBounds uBounds vBounds = uvBounds
  let uMid = Interval.midpoint uBounds
  let vMid = Interval.midpoint vBounds
  let uvCenter = UvPoint uMid vMid
  let pCenter = SurfaceFunction3D.point function uvCenter
  let error point = Quantity.abs ((point - pCenter) `dot` n)
  let maxCornerError = error p11 `max` error p12 `max` error p21 `max` error p22
  let uWidth = Interval.width uBounds
  let vWidth = Interval.width vBounds
  let uOffset = 0.5 * uWidth * Number.sqrt (3 / 7)
  let vOffset = 0.5 * vWidth * Number.sqrt (3 / 7)
  let uInterior1 = uMid - uOffset
  let uInterior2 = uMid + uOffset
  let vInterior1 = vMid - vOffset
  let vInterior2 = vMid + vOffset
  let interiorError uvPoint = error (SurfaceFunction3D.point function uvPoint)
  let interiorError11 = interiorError (UvPoint uInterior1 vInterior1)
  let interiorError21 = interiorError (UvPoint uInterior2 vInterior1)
  let interiorError12 = interiorError (UvPoint uInterior1 vInterior2)
  let interiorError22 = interiorError (UvPoint uInterior2 vInterior2)
  let maxError =
        maxCornerError
          `max` interiorError11
          `max` interiorError21
          `max` interiorError12
          `max` interiorError22
  if Resolution.acceptable (#size size) (#error maxError) resolution
    then Set2D.Leaf uvBounds uvBounds
    else do
      let Interval u1 u2 = uBounds
      let Interval v1 v2 = vBounds
      let pMid1 = SurfaceFunction3D.point function (UvPoint uMid v1)
      let pMid2 = SurfaceFunction3D.point function (UvPoint uMid v2)
      let p1Mid = SurfaceFunction3D.point function (UvPoint u1 vMid)
      let p2Mid = SurfaceFunction3D.point function (UvPoint u2 vMid)
      let uBounds1 = Interval u1 uMid
      let uBounds2 = Interval uMid u2
      let vBounds1 = Interval v1 vMid
      let vBounds2 = Interval vMid v2
      let uvBounds11 = UvBounds uBounds1 vBounds1
      let uvBounds21 = UvBounds uBounds2 vBounds1
      let uvBounds12 = UvBounds uBounds1 vBounds2
      let uvBounds22 = UvBounds uBounds2 vBounds2
      let set11 = boundarySurfaceSegmentSet resolution function uvBounds11 p11 pMid1 p1Mid pCenter
      let set21 = boundarySurfaceSegmentSet resolution function uvBounds21 pMid1 p21 pCenter p2Mid
      let set12 = boundarySurfaceSegmentSet resolution function uvBounds12 p1Mid pCenter p12 pMid2
      let set22 = boundarySurfaceSegmentSet resolution function uvBounds22 pCenter p2Mid pMid2 p22
      let set1 = Set2D.Node (Bounds2D uBounds vBounds1) set11 set21
      let set2 = Set2D.Node (Bounds2D uBounds vBounds2) set12 set22
      Set2D.Node uvBounds set1 set2

addBoundaryInnerEdgeVertices ::
  Resolution Meters ->
  Map SurfaceId (Set2D Unitless UvBounds) ->
  BoundarySurface space ->
  Map HalfEdgeId (List UvPoint) ->
  Map HalfEdgeId (List UvPoint)
addBoundaryInnerEdgeVertices resolution surfaceSegmentsById boundarySurface accumulated = do
  let BoundarySurface{edgeLoops} = boundarySurface
  NonEmpty.foldr (addLoopInnerEdgeVertices resolution surfaceSegmentsById) accumulated edgeLoops

addLoopInnerEdgeVertices ::
  Resolution Meters ->
  Map SurfaceId (Set2D Unitless UvBounds) ->
  NonEmpty (Edge space) ->
  Map HalfEdgeId (List UvPoint) ->
  Map HalfEdgeId (List UvPoint)
addLoopInnerEdgeVertices resolution surfaceSegmentsById loop accumulated =
  NonEmpty.foldr (addInnerEdgeVertices resolution surfaceSegmentsById) accumulated loop

addInnerEdgeVertices ::
  Resolution Meters ->
  Map SurfaceId (Set2D Unitless UvBounds) ->
  Edge space ->
  Map HalfEdgeId (List UvPoint) ->
  Map HalfEdgeId (List UvPoint)
addInnerEdgeVertices resolution surfaceSegmentsById edge accumulated = do
  case edge of
    DegenerateEdge{halfEdgeId, uvCurve} -> do
      let HalfEdgeId{surfaceId} = halfEdgeId
      case Map.get surfaceId surfaceSegmentsById of
        Just surfaceSegments -> do
          let edgePredicate = degenerateEdgeLinearizationPredicate uvCurve surfaceSegments
          let tValues = Domain1D.innerSamplingPoints edgePredicate
          Map.set halfEdgeId (List.map (Curve2D.point uvCurve) tValues) accumulated
        Nothing ->
          throw (InternalError "Should always be able to look up surface segments for a given edge")
    PrimaryEdge{halfEdgeId, matingId, curve3D, uvCurve, matingUvCurve, correctlyAligned} -> do
      let HalfEdgeId{surfaceId} = halfEdgeId
      let HalfEdgeId{surfaceId = matingSurfaceId} = matingId
      case (Map.get surfaceId surfaceSegmentsById, Map.get matingSurfaceId surfaceSegmentsById) of
        (Just surfaceSegments, Just matingSurfaceSegments) -> do
          let edgePredicate =
                edgeLinearizationPredicate
                  resolution
                  curve3D
                  uvCurve
                  matingUvCurve
                  correctlyAligned
                  surfaceSegments
                  matingSurfaceSegments
          let tValues = Domain1D.innerSamplingPoints edgePredicate
          let matingTValues = if correctlyAligned then List.reverseMap (1.0 -) tValues else tValues
          let vertices = List.map (Curve2D.point uvCurve) tValues
          let matingVertices = List.map (Curve2D.point matingUvCurve) matingTValues
          accumulated
            & Map.set halfEdgeId vertices
            & Map.set matingId matingVertices
        _ ->
          throw (InternalError "Should always be able to look up surface segments for a given edge")
    SecondaryEdge{} -> accumulated

edgeLinearizationPredicate ::
  Resolution Meters ->
  Curve3D space ->
  Curve2D Unitless ->
  Curve2D Unitless ->
  Bool ->
  Set2D Unitless UvBounds ->
  Set2D Unitless UvBounds ->
  Interval Unitless ->
  Bool
edgeLinearizationPredicate
  resolution
  curve3D
  uvCurve
  matingUvCurve
  correctlyAligned
  surfaceSegments
  matingSurfaceSegments
  tBounds = do
    let Interval tStart tEnd = tBounds
    let uvStart = Curve2D.point uvCurve tStart
    let uvEnd = Curve2D.point uvCurve tEnd
    let matingTStart = if correctlyAligned then 1.0 - tStart else tStart
    let matingTEnd = if correctlyAligned then 1.0 - tEnd else tEnd
    let matingUvStart = Curve2D.point matingUvCurve matingTStart
    let matingUvEnd = Curve2D.point matingUvCurve matingTEnd
    let uvBounds = Bounds2D.hull2 uvStart uvEnd
    let matingUvBounds = Bounds2D.hull2 matingUvStart matingUvEnd
    let edgeSize = Point2D.distanceFrom uvStart uvEnd
    let matingEdgeSize = Point2D.distanceFrom matingUvStart matingUvEnd
    let startPoint = Curve3D.point curve3D tStart
    let endPoint = Curve3D.point curve3D tEnd
    let edgeLength = Point3D.distanceFrom startPoint endPoint
    let edgeLinearDeviation = Curve.linearDeviation curve3D tBounds
    Resolution.acceptable (#size edgeLength) (#error edgeLinearDeviation) resolution
      && validEdge uvBounds edgeSize surfaceSegments
      && validEdge matingUvBounds matingEdgeSize matingSurfaceSegments

degenerateEdgeLinearizationPredicate ::
  Curve2D Unitless ->
  Set2D Unitless UvBounds ->
  Interval Unitless ->
  Bool
degenerateEdgeLinearizationPredicate uvCurve surfaceSegments tBounds = do
  let Interval tStart tEnd = tBounds
  let uvStart = Curve2D.point uvCurve tStart
  let uvEnd = Curve2D.point uvCurve tEnd
  let uvBounds = Bounds2D.hull2 uvStart uvEnd
  let edgeSize = Point2D.distanceFrom uvStart uvEnd
  validEdge uvBounds edgeSize surfaceSegments

validEdge :: UvBounds -> Number -> Set2D Unitless UvBounds -> Bool
validEdge edgeBounds edgeLength surfaceSegments = Tolerance.using Quantity.zero do
  case surfaceSegments of
    Set2D.Node nodeBounds left right ->
      not (edgeBounds `intersects` nodeBounds)
        || (validEdge edgeBounds edgeLength left && validEdge edgeBounds edgeLength right)
    Set2D.Leaf leafBounds _ ->
      not (edgeBounds `intersects` leafBounds)
        || edgeLength <= Number.sqrt 2.0 * Bounds2D.diameter leafBounds

boundarySurfaceMesh ::
  Tolerance Meters =>
  Map SurfaceId (Set2D Unitless UvBounds) ->
  Map HalfEdgeId (List UvPoint) ->
  ToVertex vertex space ->
  BoundarySurface space ->
  Mesh vertex
boundarySurfaceMesh surfaceSegmentsById innerEdgeVerticesById toVertex boundarySurface = do
  let BoundarySurface{surfaceId, handedness, edgeLoops} = boundarySurface
  case SurfaceFunction3D.nondegenerate boundarySurface.surfaceFunction of
    Error IsDegenerate -> Mesh.empty
    Ok surfaceFunction -> do
      let boundaryPolygons = NonEmpty.map (toPolygon innerEdgeVerticesById) edgeLoops
      let boundarySegments = NonEmpty.combine Polygon2D.edges boundaryPolygons
      let boundarySegmentSet = Set2D.partitionBy Line2D.bounds boundarySegments
      case Map.get surfaceId surfaceSegmentsById of
        Nothing -> InternalError.throw "Should always be able to look up surface segments by ID"
        Just surfaceSegments -> do
          let steinerPoints =
                case surfaceSegments of
                  Set2D.Leaf{} ->
                    -- Special case, if the surface as a whole is sufficiently linear
                    -- (only needs a single segment to approximate it)
                    -- then we don't need any interior points at all
                    -- (sufficient to just use the boundary points)
                    []
                  Set2D.Node{} ->
                    List.filterMap (steinerPoint boundarySegmentSet) (Set2D.toList surfaceSegments)
          let boundaryVertexLoops = NonEmpty.map Polygon2D.vertices boundaryPolygons
          -- Decent refinement option: (Just (List.length steinerPoints, steinerVertex))
          let uvPointMesh = CDT.unsafe boundaryVertexLoops steinerPoints
          let uvPoints = Mesh.vertices uvPointMesh
          let vertices = Array.map (toVertex surfaceFunction handedness) uvPoints
          let faceIndices =
                case handedness of
                  Positive -> Mesh.faceIndices uvPointMesh
                  Negative -> List.map (\(i, j, k) -> (k, j, i)) (Mesh.faceIndices uvPointMesh)
          Mesh.indexed vertices faceIndices

toPolygon :: Map HalfEdgeId (List UvPoint) -> NonEmpty (Edge space) -> Polygon2D Unitless
toPolygon innerEdgeVerticesById loop =
  Polygon2D (NonEmpty.combine (leadingEdgeVertices innerEdgeVerticesById) loop)

leadingEdgeVertices ::
  Map HalfEdgeId (List UvPoint) ->
  Edge space ->
  NonEmpty UvPoint
leadingEdgeVertices innerEdgeVerticesById edge = case edge of
  DegenerateEdge{halfEdgeId, uvCurve} ->
    leadingEdgeVerticesImpl innerEdgeVerticesById halfEdgeId (Curve2D.startPoint uvCurve)
  PrimaryEdge{halfEdgeId, uvCurve} ->
    leadingEdgeVerticesImpl innerEdgeVerticesById halfEdgeId (Curve2D.startPoint uvCurve)
  SecondaryEdge{halfEdgeId, uvStartPoint} ->
    leadingEdgeVerticesImpl innerEdgeVerticesById halfEdgeId uvStartPoint

leadingEdgeVerticesImpl ::
  Map HalfEdgeId (List UvPoint) ->
  HalfEdgeId ->
  UvPoint ->
  NonEmpty UvPoint
leadingEdgeVerticesImpl innerEdgeVerticesById edgeId uvStartPoint =
  case Map.get edgeId innerEdgeVerticesById of
    Just innerEdgeVertices -> uvStartPoint :| innerEdgeVertices
    Nothing -> InternalError.throw "Should always be able to look up internal edge vertices by ID"

steinerPoint :: Set2D Unitless (Line2D Unitless) -> UvBounds -> Maybe UvPoint
steinerPoint boundarySegmentSet uvBounds = do
  let Bounds2D uBounds vBounds = uvBounds
  let uvPoint = UvPoint (Interval.midpoint uBounds) (Interval.midpoint vBounds)
  if isValidSteinerPoint boundarySegmentSet uvPoint then Just uvPoint else Nothing

isValidSteinerPoint :: Set2D Unitless (Line2D Unitless) -> UvPoint -> Bool
isValidSteinerPoint edgeSet uvPoint = case edgeSet of
  Set2D.Node nodeBounds left right ->
    case Bounds2D.exclusion# uvPoint nodeBounds >=# 0.5## *# Bounds2D.diameter# nodeBounds of
      1# -> True
      _ -> isValidSteinerPoint left uvPoint && isValidSteinerPoint right uvPoint
  Set2D.Leaf _ edge -> Line2D.distanceTo uvPoint edge >= 0.5 * Line2D.length edge

surfaces :: Body3D space -> NonEmpty (Surface3D space)
surfaces (Body3D boundarySurfaces) = NonEmpty.map (.orientedSurface) boundarySurfaces

-- | Convert a body defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3D global local -> Body3D local -> Body3D global
placeIn frame (Body3D boundarySurfaces) =
  Body3D (NonEmpty.map (placeBoundarySurfaceIn frame) boundarySurfaces)

placeBoundarySurfaceIn ::
  Frame3D global local ->
  BoundarySurface local ->
  BoundarySurface global
placeBoundarySurfaceIn frame boundarySurface = do
  let BoundarySurface
        { surfaceId
        , orientedSurface
        , surfaceFunction
        , handedness
        , uvBounds
        , edgeLoops
        } = boundarySurface
  BoundarySurface
    { surfaceId
    , orientedSurface = Surface3D.placeIn frame orientedSurface
    , surfaceFunction = SurfaceFunction3D.placeIn frame surfaceFunction
    , handedness
    , uvBounds
    , edgeLoops = NonEmpty.map (NonEmpty.map (placeEdgeIn frame)) edgeLoops
    }

placeEdgeIn :: Frame3D global local -> Edge local -> Edge global
placeEdgeIn frame edge = case edge of
  PrimaryEdge{halfEdgeId, uvCurve, curve3D, matingId, matingUvCurve, correctlyAligned} ->
    PrimaryEdge
      { halfEdgeId
      , uvCurve
      , curve3D = Curve3D.placeIn frame curve3D
      , matingId
      , matingUvCurve
      , correctlyAligned
      }
  SecondaryEdge{halfEdgeId, uvStartPoint} ->
    SecondaryEdge{halfEdgeId, uvStartPoint}
  DegenerateEdge{halfEdgeId, uvCurve} ->
    DegenerateEdge{halfEdgeId, uvCurve}

-- | Convert a body defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3D global local -> Body3D global -> Body3D local
relativeTo frame body = placeIn (Frame3D.inverse frame) body
