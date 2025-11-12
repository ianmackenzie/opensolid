{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module OpenSolid.Body3d
  ( Body3d
  , block
  , sphere
  , cylinder
  , cylinderAlong
  , extruded
  , translational
  , revolved
  , boundedBy
  , toMesh
  , surfaces
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Array qualified as Array
import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Axis3d (Axis3d (Axis3d))
import OpenSolid.Axis3d qualified as Axis3d
import OpenSolid.Body3d.BoundedBy qualified as BoundedBy
import OpenSolid.Bounded2d (Bounded2d)
import OpenSolid.Bounded2d qualified
import OpenSolid.Bounded3d (Bounded3d)
import OpenSolid.Bounded3d qualified as Bounded3d
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Bounds3d (Bounds3d)
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.CDT qualified as CDT
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Curve3d (Curve3d)
import OpenSolid.Curve3d qualified as Curve3d
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.DirectionSurfaceFunction3d (DirectionSurfaceFunction3d)
import OpenSolid.DirectionSurfaceFunction3d qualified as DirectionSurfaceFunction3d
import OpenSolid.Domain1d qualified as Domain1d
import OpenSolid.Error qualified as Error
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.InternalError (InternalError (InternalError))
import OpenSolid.LineSegment2d (LineSegment2d)
import OpenSolid.LineSegment2d qualified as LineSegment2d
import OpenSolid.Linearization qualified as Linearization
import OpenSolid.List qualified as List
import OpenSolid.Map (Map)
import OpenSolid.Map qualified as Map
import OpenSolid.Mesh (Mesh)
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Plane3d (Plane3d (Plane3d))
import OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Polygon2d (Polygon2d (Polygon2d))
import OpenSolid.Polygon2d qualified as Polygon2d
import OpenSolid.Prelude
import OpenSolid.Quantity (Quantity (Quantity##))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Region2d (Region2d)
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Resolution (Resolution)
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Set2d (Set2d)
import OpenSolid.Set2d qualified as Set2d
import OpenSolid.Set3d (Set3d)
import OpenSolid.Set3d qualified as Set3d
import OpenSolid.Surface3d (Surface3d)
import OpenSolid.Surface3d qualified as Surface3d
import OpenSolid.Surface3d.Revolved qualified as Surface3d.Revolved
import OpenSolid.SurfaceFunction3d (SurfaceFunction3d)
import OpenSolid.SurfaceFunction3d qualified as SurfaceFunction3d
import OpenSolid.SurfaceLinearization qualified as SurfaceLinearization
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Unboxed.Math
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds3d qualified as VectorBounds3d
import OpenSolid.VectorCurve3d (VectorCurve3d)
import OpenSolid.VectorCurve3d qualified as VectorCurve3d
import OpenSolid.VectorSurfaceFunction3d qualified as VectorSurfaceFunction3d
import OpenSolid.Vertex2d as Vertex2d (Vertex2d)
import OpenSolid.Vertex2d qualified as Vertex2d
import OpenSolid.Vertex3d (Vertex3d)
import OpenSolid.Vertex3d qualified as Vertex3d
import OpenSolid.World3d qualified as World3d

-- | A solid body in 3D, defined by a set of boundary surfaces.
newtype Body3d (coordinateSystem :: CoordinateSystem)
  = Body3d (NonEmpty (BoundarySurface coordinateSystem))

instance FFI (Body3d FFI.Coordinates) where
  representation = FFI.classRepresentation "Body3d"

data BoundarySurface (coordinateSystem :: CoordinateSystem) where
  BoundarySurface ::
    { surfaceId :: SurfaceId
    , orientedSurface :: Surface3d (space @ units)
    , surfaceFunction :: SurfaceFunction3d (space @ units)
    , handedness :: Sign
    , uvBounds :: UvBounds
    , edgeLoops :: NonEmpty (NonEmpty (Edge (space @ units)))
    } ->
    BoundarySurface (space @ units)

newtype SurfaceId = SurfaceId Int deriving (Eq, Ord, Show)

data Edge (coordinateSystem :: CoordinateSystem) where
  PrimaryEdge ::
    { halfEdgeId :: HalfEdgeId
    , startPoint :: Point3d (space @ units)
    , uvCurve :: Curve2d UvCoordinates
    , curve3d :: Curve3d (space @ units)
    , matingId :: HalfEdgeId
    , matingUvCurve :: Curve2d UvCoordinates
    , correctlyAligned :: Bool
    } ->
    Edge (space @ units)
  SecondaryEdge ::
    { halfEdgeId :: HalfEdgeId
    , startPoint :: Point3d (space @ units)
    , uvStartPoint :: UvPoint
    } ->
    Edge (space @ units)
  DegenerateEdge ::
    { halfEdgeId :: HalfEdgeId
    , point :: Point3d (space @ units)
    , uvCurve :: Curve2d UvCoordinates
    } ->
    Edge (space @ units)

data HalfEdgeId = HalfEdgeId
  { surfaceId :: SurfaceId
  , loopId :: LoopId
  , curveId :: CurveId
  }
  deriving (Eq, Ord, Show)

newtype LoopId = LoopId Int deriving (Eq, Ord, Show)

newtype CurveId = CurveId Int deriving (Eq, Ord, Show)

----- CONSTRUCTION -----

data SurfaceWithHalfEdges (coordinateSystem :: CoordinateSystem) where
  SurfaceWithHalfEdges ::
    { surfaceId :: SurfaceId
    , surface :: Surface3d (space @ units)
    , handedness :: Sign
    , halfEdgeLoops :: NonEmpty (NonEmpty (HalfEdge (space @ units)))
    } ->
    SurfaceWithHalfEdges (space @ units)

data HalfEdge (coordinateSystem :: CoordinateSystem) where
  HalfEdge ::
    { halfEdgeId :: HalfEdgeId
    , uvCurve :: Curve2d UvCoordinates -- UV curve parameterized by 3D arc length
    , curve3d :: Curve3d (space @ units) -- Arc length parameterized 3D curve
    , length :: Quantity units -- Arc length of 3D curve
    , bounds :: Bounds3d (space @ units) -- Bounds on 3D curve
    } ->
    HalfEdge (space @ units)
  DegenerateHalfEdge ::
    { halfEdgeId :: HalfEdgeId
    , uvCurve :: Curve2d UvCoordinates
    , point :: Point3d (space @ units)
    } ->
    HalfEdge (space @ units)

instance Bounded3d (HalfEdge (space @ units)) (space @ units) where
  bounds HalfEdge{bounds} = bounds
  bounds DegenerateHalfEdge{point} = Bounds3d.constant point

data MatingEdge = MatingEdge
  { halfEdgeId :: HalfEdgeId
  , uvCurve :: Curve2d UvCoordinates
  , correctlyAligned :: Bool
  }

data SurfaceRegistry (coordinateSystem :: CoordinateSystem) where
  SurfaceRegistry ::
    { unprocessed :: Map SurfaceId (SurfaceWithHalfEdges (space @ units))
    , processed :: Map SurfaceId (SurfaceWithHalfEdges (space @ units))
    , edges :: Map HalfEdgeId (Edge (space @ units))
    } ->
    SurfaceRegistry (space @ units)

data Corner (coordinateSystem :: CoordinateSystem) where
  Corner ::
    { surfaceId :: SurfaceId
    , point :: Point3d (space @ units)
    } ->
    Corner (space @ units)

instance Bounded3d (Corner (space @ units)) (space @ units) where
  bounds Corner{point} = Bounds3d.constant point

data EmptyBody = EmptyBody deriving (Eq, Show, Error.Message)

{-| Create a rectangular block body.

Fails if the given bounds are empty
(the length, width, or height is zero).
-}
block :: Tolerance units => Bounds3d (space @ units) -> Result EmptyBody (Body3d (space @ units))
block bounds =
  case Region2d.rectangle (Bounds3d.projectInto World3d.topPlane bounds) of
    Failure Region2d.EmptyRegion -> Failure EmptyBody
    Success profile -> do
      let Bounds h1 h2 = Bounds3d.upwardCoordinate bounds
      if h1 ~= h2
        then Failure EmptyBody
        else case extruded World3d.topPlane profile h1 h2 of
          Success body -> Success body
          Failure _ ->
            throw (InternalError "Constructing block body from non-empty bounds should not fail")

{-| Create a sphere with the given center point and diameter.

Fails if the diameter is zero.
-}
sphere ::
  Tolerance units =>
  "centerPoint" ::: Point3d (space @ units) ->
  "diameter" ::: Quantity units ->
  Result EmptyBody (Body3d (space @ units))
sphere (Named centerPoint) (Named diameter) =
  if diameter ~= Quantity.zero
    then Failure EmptyBody
    else do
      let sketchPlane = Plane3d centerPoint World3d.frontPlane.orientation
      let radius = 0.5 *. diameter
      let p1 = Point2d.y (negative radius)
      let p2 = Point2d.y radius
      let profileCurves = [Curve2d.arc p1 p2 Angle.pi, Curve2d.line p2 p1]
      case Region2d.boundedBy profileCurves of
        Failure _ -> throw (InternalError "Semicircle profile construction should always succeed")
        Success profile ->
          case revolved sketchPlane profile Axis2d.y Angle.twoPi of
            Success body -> Success body
            Failure _ ->
              throw (InternalError "Constructing sphere from non-zero radius should not fail")

{-| Create a cylindrical body from a start point, end point and diameter.

Fails if the cylinder length or diameter is zero.
-}
cylinder ::
  Tolerance units =>
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  "diameter" ::: Quantity units ->
  Result EmptyBody (Body3d (space @ units))
cylinder startPoint endPoint (Named diameter) =
  case Vector3d.magnitudeAndDirection (endPoint .-. startPoint) of
    Failure Vector3d.IsZero -> Failure EmptyBody
    Success (length, direction) ->
      cylinderAlong (Axis3d startPoint direction) Quantity.zero length (#diameter diameter)

{-| Create a cylindrical body along a given axis.

In addition to the axis itself, you will need to provide:

- Where along the axis the cylinder starts and ends
  (given as a range of distances along the axis).
- The cylinder diameter.

Failes if the cylinder length or diameter is zero.
-}
cylinderAlong ::
  Tolerance units =>
  Axis3d (space @ units) ->
  Quantity units ->
  Quantity units ->
  "diameter" ::: Quantity units ->
  Result EmptyBody (Body3d (space @ units))
cylinderAlong axis d1 d2 (Named diameter) = do
  case Region2d.circle (#centerPoint Point2d.origin) (#diameter diameter) of
    Failure Region2d.EmptyRegion -> Failure EmptyBody
    Success profile ->
      if d1 ~= d2
        then Failure EmptyBody
        else case extruded (Axis3d.normalPlane axis) profile d1 d2 of
          Success body -> Success body
          Failure _ -> throw (InternalError "Constructing non-empty cylinder body should not fail")

-- | Create an extruded body from a sketch plane and profile.
extruded ::
  Tolerance units =>
  Plane3d (space @ units) (Defines local) ->
  Region2d (local @ units) ->
  Quantity units ->
  Quantity units ->
  Result BoundedBy.Error (Body3d (space @ units))
extruded sketchPlane profile d1 d2 = do
  let normal = Plane3d.normalDirection sketchPlane
  let v1 = d1 .*. normal
  let v2 = d2 .*. normal
  translational sketchPlane profile (VectorCurve3d.line v1 v2)

translational ::
  Tolerance units =>
  Plane3d (space @ units) (Defines local) ->
  Region2d (local @ units) ->
  VectorCurve3d (space @ units) ->
  Result BoundedBy.Error (Body3d (space @ units))
translational sketchPlane profile displacement = do
  let v0 = VectorCurve3d.startValue displacement
  let v1 = VectorCurve3d.endValue displacement
  let startPlane = Plane3d.translateBy v0 sketchPlane
  let endPlane = Plane3d.translateBy v1 sketchPlane
  let startCap = Surface3d.on startPlane profile
  let endCap = Surface3d.on endPlane profile
  let sideSurface curve = Surface3d.translational (Curve2d.placeOn sketchPlane curve) displacement
  let sideSurfaces = List.map sideSurface (NonEmpty.toList (Region2d.boundaryCurves profile))
  let initialDerivative = VectorCurve3d.startValue displacement.derivative
  case Quantity.sign (initialDerivative `dot` Plane3d.normalDirection sketchPlane) of
    Positive -> boundedBy (endCap : startCap : sideSurfaces)
    Negative -> boundedBy (startCap : endCap : sideSurfaces)

{-| Create a revolved body from a sketch plane and profile.

Note that the revolution profile and revolution axis
are both defined within the given sketch plane.

A positive angle will result in a counterclockwise revolution around the axis,
and a negative angle will result in a clockwise revolution.
-}
revolved ::
  Tolerance units =>
  Plane3d (space @ units) (Defines local) ->
  Region2d (local @ units) ->
  Axis2d (local @ units) ->
  Angle ->
  Result BoundedBy.Error (Body3d (space @ units))
revolved startPlane profile axis2d angle = do
  let axis3d = Axis2d.placeOn startPlane axis2d
  let profileCurves = Region2d.boundaryCurves profile
  let offAxisCurves = NonEmpty.filter (not . Curve2d.isOnAxis axis2d) profileCurves
  let signedDistanceCurves = List.map (Curve2d.distanceRightOf axis2d) offAxisCurves
  profileSign <-
    case Result.collect Curve.sign signedDistanceCurves of
      Failure Curve.CrossesZero -> Failure BoundedBy.BoundaryIntersectsItself
      Success curveSigns
        | List.allSatisfy (== Positive) curveSigns -> Success Positive
        | List.allSatisfy (== Negative) curveSigns -> Success Negative
        | otherwise -> Failure BoundedBy.BoundaryIntersectsItself

  let endPlane = Plane3d.rotateAround axis3d angle startPlane
  let unflippedStartCap = Surface3d.on startPlane profile
  let unflippedEndCap = Surface3d.on endPlane profile
  let sideSurface profileCurve = Surface3d.revolved startPlane profileCurve axis2d angle
  sideSurfaces <-
    case Result.collect sideSurface offAxisCurves of
      Failure Surface3d.Revolved.ProfileIsOnAxis ->
        throw (InternalError "Should have already filtered out all on-axis curves")
      Failure Surface3d.Revolved.ProfileCrossesAxis ->
        throw (InternalError "Should have already failed computing profileSign if any profile curve crosses the revolution axis")
      Success surface -> Success surface
  let startBoundaryCurves = Surface3d.boundaryCurves unflippedStartCap
  let endBoundaryCurves = Surface3d.boundaryCurves unflippedEndCap
  let isFullRevolution = startBoundaryCurves ~= endBoundaryCurves
  let endSurfaces =
        if isFullRevolution
          then []
          else case profileSign of
            Positive -> [unflippedStartCap, Surface3d.flip unflippedEndCap]
            Negative -> [Surface3d.flip unflippedStartCap, unflippedEndCap]
  boundedBy (endSurfaces <> sideSurfaces)

{-| Create a body bounded by the given surfaces.
The surfaces do not have to have consistent orientation,
but currently the *first* surface must have the correct orientation
since all others will be flipped if necessary to match it.
-}
boundedBy ::
  Tolerance units =>
  List (Surface3d (space @ units)) ->
  Result BoundedBy.Error (Body3d (space @ units))
boundedBy [] = Failure BoundedBy.EmptyBody
boundedBy (NonEmpty givenSurfaces) = do
  let surfacesWithHalfEdges = NonEmpty.mapWithIndex toSurfaceWithHalfEdges givenSurfaces
  let firstSurfaceWithHalfEdges = NonEmpty.first surfacesWithHalfEdges
  let halfEdges = NonEmpty.combine getAllHalfEdges surfacesWithHalfEdges
  let halfEdgeSet = Set3d.fromNonEmpty halfEdges
  let corners = NonEmpty.map halfEdgeStartPoint halfEdges
  let cornerSet = Set3d.fromNonEmpty corners
  let initialSurfaceRegistry =
        SurfaceRegistry
          { unprocessed =
              Map.fromList surfaceWithHalfEdgesMapEntry $
                NonEmpty.toList surfacesWithHalfEdges
          , processed = Map.empty
          , edges = Map.empty
          }
  finalSurfaceRegistry <-
    registerSurfaceWithHalfEdges
      cornerSet
      halfEdgeSet
      initialSurfaceRegistry
      firstSurfaceWithHalfEdges
  let SurfaceRegistry{unprocessed, processed, edges} = finalSurfaceRegistry
  case (Map.values unprocessed, Map.values processed) of
    (NonEmpty _, _) -> Failure BoundedBy.BoundaryHasGaps
    ([], []) -> throw (InternalError "Should always have at least one processed surface")
    ([], NonEmpty processedSurfaces) ->
      Success (Body3d (NonEmpty.map (toBoundarySurface edges) processedSurfaces))

surfaceWithHalfEdgesMapEntry ::
  SurfaceWithHalfEdges (space @ units) ->
  (SurfaceId, SurfaceWithHalfEdges (space @ units))
surfaceWithHalfEdgesMapEntry surfaceWithHalfEdges = do
  let SurfaceWithHalfEdges{surfaceId} = surfaceWithHalfEdges
  (surfaceId, surfaceWithHalfEdges)

toSurfaceWithHalfEdges ::
  Tolerance units =>
  Int ->
  Surface3d (space @ units) ->
  SurfaceWithHalfEdges (space @ units)
toSurfaceWithHalfEdges surfaceIndex surface = do
  let loops = Region2d.boundaryLoops surface.domain
  let surfaceId = SurfaceId surfaceIndex
  let halfEdges = NonEmpty.mapWithIndex (loopHalfEdges surfaceId surface.function) loops
  SurfaceWithHalfEdges surfaceId surface Positive halfEdges

loopHalfEdges ::
  Tolerance units =>
  SurfaceId ->
  SurfaceFunction3d (space @ units) ->
  Int ->
  NonEmpty (Curve2d UvCoordinates) ->
  NonEmpty (HalfEdge (space @ units))
loopHalfEdges surfaceId surfaceFunction loopIndex loop =
  NonEmpty.mapWithIndex (toHalfEdge surfaceId (LoopId loopIndex) surfaceFunction) loop

toHalfEdge ::
  Tolerance units =>
  SurfaceId ->
  LoopId ->
  SurfaceFunction3d (space @ units) ->
  Int ->
  Curve2d UvCoordinates ->
  HalfEdge (space @ units)
toHalfEdge surfaceId loopId surfaceFunction curveIndex uvCurve = do
  let curve3d = surfaceFunction `compose` uvCurve
  let curveId = CurveId curveIndex
  let halfEdgeId = HalfEdgeId{surfaceId, loopId, curveId}
  let (parameterization, length) = Curve3d.arcLengthParameterization curve3d
  if length == Quantity.zero
    then DegenerateHalfEdge{halfEdgeId, uvCurve, point = Curve3d.startPoint curve3d}
    else
      HalfEdge
        { halfEdgeId
        , uvCurve = uvCurve `compose` parameterization
        , curve3d = curve3d `compose` parameterization
        , length
        , bounds = Curve3d.bounds curve3d
        }

getAllHalfEdges :: SurfaceWithHalfEdges (space @ units) -> NonEmpty (HalfEdge (space @ units))
getAllHalfEdges SurfaceWithHalfEdges{halfEdgeLoops} = NonEmpty.concat halfEdgeLoops

halfEdgeStartPoint :: HalfEdge (space @ units) -> Corner (space @ units)
halfEdgeStartPoint halfEdge = case halfEdge of
  HalfEdge{halfEdgeId = HalfEdgeId{surfaceId}, curve3d} ->
    Corner surfaceId (Curve3d.startPoint curve3d)
  DegenerateHalfEdge{halfEdgeId = HalfEdgeId{surfaceId}, point} ->
    Corner surfaceId point

registerSurfaceWithHalfEdges ::
  Tolerance units =>
  Set3d (Corner (space @ units)) (space @ units) ->
  Set3d (HalfEdge (space @ units)) (space @ units) ->
  SurfaceRegistry (space @ units) ->
  SurfaceWithHalfEdges (space @ units) ->
  Result BoundedBy.Error (SurfaceRegistry (space @ units))
registerSurfaceWithHalfEdges cornerSet halfEdgeSet surfaceRegistry surfaceWithHalfEdges = do
  let SurfaceWithHalfEdges{surfaceId, handedness} = surfaceWithHalfEdges
  let SurfaceRegistry{unprocessed, processed, edges} = surfaceRegistry
  let updatedRegistry =
        SurfaceRegistry
          { unprocessed = Map.remove surfaceId unprocessed
          , processed = Map.set surfaceId surfaceWithHalfEdges processed
          , edges
          }
  let halfEdges = getAllHalfEdges surfaceWithHalfEdges
  Result.foldl (registerHalfEdge handedness cornerSet halfEdgeSet) updatedRegistry halfEdges

registerHalfEdge ::
  Tolerance units =>
  Sign ->
  Set3d (Corner (space @ units)) (space @ units) ->
  Set3d (HalfEdge (space @ units)) (space @ units) ->
  SurfaceRegistry (space @ units) ->
  HalfEdge (space @ units) ->
  Result BoundedBy.Error (SurfaceRegistry (space @ units))
registerHalfEdge parentHandedness cornerSet halfEdgeSet surfaceRegistry halfEdge = do
  let SurfaceRegistry{unprocessed, processed, edges} = surfaceRegistry
  case halfEdge of
    DegenerateHalfEdge{halfEdgeId, uvCurve, point} -> do
      canonicalPoint <- getCornerPoint point cornerSet
      let edge = DegenerateEdge{halfEdgeId, uvCurve, point = canonicalPoint}
      Success SurfaceRegistry{unprocessed, processed, edges = Map.set halfEdgeId edge edges}
    HalfEdge{halfEdgeId, uvCurve, curve3d} -> do
      let matingEdgeCandidates = Set3d.filter (Bounded3d.bounds halfEdge) halfEdgeSet
      case List.filterMap (toMatingEdge halfEdgeId curve3d) matingEdgeCandidates of
        [] -> Failure BoundedBy.BoundaryHasGaps
        List.TwoOrMore -> Failure BoundedBy.BoundaryIntersectsItself
        List.One MatingEdge{halfEdgeId = matingId, uvCurve = matingUvCurve, correctlyAligned} -> do
          let HalfEdgeId{surfaceId = matingSurfaceId} = matingId
          startPoint <- getCornerPoint (Curve3d.startPoint curve3d) cornerSet
          let edge =
                if halfEdgeId < matingId
                  then
                    PrimaryEdge
                      { halfEdgeId
                      , startPoint
                      , uvCurve
                      , curve3d
                      , matingId
                      , matingUvCurve
                      , correctlyAligned
                      }
                  else SecondaryEdge{halfEdgeId, startPoint, uvStartPoint = uvCurve.startPoint}
          let updatedRegistry =
                SurfaceRegistry{unprocessed, processed, edges = Map.set halfEdgeId edge edges}
          let matingHandedness = if correctlyAligned then parentHandedness else negative parentHandedness
          case (Map.get matingSurfaceId unprocessed, Map.get matingSurfaceId processed) of
            (Nothing, Nothing) -> throw (InternalError "No surface found for half-edge")
            (Just _, Just _) -> throw (InternalError "Multiple surfaces found for half-edge")
            (Nothing, Just SurfaceWithHalfEdges{handedness}) ->
              if handedness == matingHandedness
                then Success updatedRegistry
                else Failure BoundedBy.BoundaryIntersectsItself
            (Just unprocessedSurface, Nothing) -> do
              let processedSurface = setHandedness matingHandedness unprocessedSurface
              registerSurfaceWithHalfEdges cornerSet halfEdgeSet updatedRegistry processedSurface

setHandedness ::
  Sign ->
  SurfaceWithHalfEdges (space @ units) ->
  SurfaceWithHalfEdges (space @ units)
setHandedness handedness SurfaceWithHalfEdges{surfaceId, surface, halfEdgeLoops} =
  SurfaceWithHalfEdges{handedness, surfaceId, surface, halfEdgeLoops}

toBoundarySurface ::
  Map HalfEdgeId (Edge (space @ units)) ->
  SurfaceWithHalfEdges (space @ units) ->
  BoundarySurface (space @ units)
toBoundarySurface edges SurfaceWithHalfEdges{surfaceId, surface, handedness, halfEdgeLoops} =
  BoundarySurface
    { surfaceId
    , orientedSurface = case handedness of
        Positive -> surface
        Negative -> Surface3d.flip surface
    , surfaceFunction = surface.function
    , handedness
    , uvBounds = Region2d.bounds surface.domain
    , edgeLoops = NonEmpty.map (NonEmpty.map (toEdge edges)) halfEdgeLoops
    }

toEdge :: Map HalfEdgeId (Edge (space @ units)) -> HalfEdge (space @ units) -> Edge (space @ units)
toEdge edges halfEdge =
  case Map.get (getHalfEdgeId halfEdge) edges of
    Just edge -> edge
    Nothing -> throw (InternalError "Should always be able to find edge for processed half-edge")

getHalfEdgeId :: HalfEdge (space @ units) -> HalfEdgeId
getHalfEdgeId HalfEdge{halfEdgeId} = halfEdgeId
getHalfEdgeId DegenerateHalfEdge{halfEdgeId} = halfEdgeId

cornerSurfaceId :: Corner (space @ units) -> SurfaceId
cornerSurfaceId Corner{surfaceId} = surfaceId

cornerPoint :: Corner (space @ units) -> Point3d (space @ units)
cornerPoint Corner{point} = point

getCornerPoint ::
  Tolerance units =>
  Point3d (space @ units) ->
  Set3d (Corner (space @ units)) (space @ units) ->
  Result BoundedBy.Error (Point3d (space @ units))
getCornerPoint searchPoint cornerSet =
  case Set3d.filter (Bounds3d.constant searchPoint) cornerSet of
    [] -> throw (InternalError "getCorner should always find at least one corner (the given point itself)")
    NonEmpty candidates -> Success (cornerPoint (NonEmpty.minimumBy cornerSurfaceId candidates))

toMatingEdge ::
  Tolerance units =>
  HalfEdgeId ->
  Curve3d (space @ units) ->
  HalfEdge (space @ units) ->
  Maybe MatingEdge
toMatingEdge _ _ DegenerateHalfEdge{} = Nothing
toMatingEdge id1 curve1 HalfEdge{halfEdgeId = id2, curve3d = curve2, uvCurve}
  | id1 == id2 = Nothing
  | otherwise = do
      let points1 = List.map (Curve3d.evaluate curve1) Parameter.samples
      let points2 = List.map (Curve3d.evaluate curve2) Parameter.samples
      if
        | points1 ~= points2 ->
            Just MatingEdge{halfEdgeId = id2, uvCurve, correctlyAligned = False}
        | points1 ~= List.reverse points2 ->
            Just MatingEdge{halfEdgeId = id2, uvCurve, correctlyAligned = True}
        | otherwise -> Nothing

----- MESHING -----

data Vertex (coordinateSystem :: CoordinateSystem) where
  Vertex :: UvPoint -> Point3d (space @ units) -> Vertex (space @ units)

deriving instance Eq (Vertex (space @ units))

instance Vertex2d (Vertex (space @ units)) UvCoordinates where
  position (Vertex uvPoint _) = uvPoint

instance Vertex3d (Vertex (space @ units)) (space @ units) where
  position (Vertex _ point) = point

instance Bounded2d (Vertex (space @ units)) UvCoordinates where
  bounds (Vertex uvPoint _) = Bounds2d.constant uvPoint

toMesh ::
  Tolerance units =>
  Resolution units ->
  Body3d (space @ units) ->
  Mesh (Point3d (space @ units), Direction3d space)
toMesh resolution (Body3d boundarySurfaces) = do
  let boundarySurfaceList = NonEmpty.toList boundarySurfaces
  let surfaceSegmentsById = Map.fromList (boundarySurfaceSegments resolution) boundarySurfaceList
  let innerEdgeVerticesById =
        NonEmpty.foldr
          (addBoundaryInnerEdgeVertices resolution surfaceSegmentsById)
          Map.empty
          boundarySurfaces
  Mesh.combine (boundarySurfaceMesh surfaceSegmentsById innerEdgeVerticesById) boundarySurfaces

boundarySurfaceSegments ::
  Resolution units ->
  BoundarySurface (space @ units) ->
  (SurfaceId, Set2d UvBounds UvCoordinates)
boundarySurfaceSegments resolution BoundarySurface{surfaceId, surfaceFunction, uvBounds} =
  (surfaceId, boundarySurfaceSegmentSet resolution surfaceFunction uvBounds)

boundarySurfaceSegmentSet ::
  Resolution units ->
  SurfaceFunction3d (space @ units) ->
  UvBounds ->
  Set2d UvBounds UvCoordinates
boundarySurfaceSegmentSet resolution surfaceFunction uvBounds = do
  let acceptableSize =
        Quantity.isInfinite resolution.maxSize
          || surfaceSize surfaceFunction uvBounds <= resolution.maxSize
  let acceptableError =
        Quantity.isInfinite resolution.maxError
          || surfaceError surfaceFunction uvBounds <= resolution.maxError
  if acceptableSize && acceptableError
    then Set2d.Leaf uvBounds uvBounds
    else do
      let Bounds2d uBounds vBounds = uvBounds
      let (u1, u2) = Bounds.bisect uBounds
      let (v1, v2) = Bounds.bisect vBounds
      let set11 = boundarySurfaceSegmentSet resolution surfaceFunction (Bounds2d u1 v1)
      let set12 = boundarySurfaceSegmentSet resolution surfaceFunction (Bounds2d u1 v2)
      let set21 = boundarySurfaceSegmentSet resolution surfaceFunction (Bounds2d u2 v1)
      let set22 = boundarySurfaceSegmentSet resolution surfaceFunction (Bounds2d u2 v2)
      let set1 = Set2d.Node (Bounds2d u1 vBounds) set11 set12
      let set2 = Set2d.Node (Bounds2d u2 vBounds) set21 set22
      Set2d.Node uvBounds set1 set2

surfaceSize :: SurfaceFunction3d (space @ units) -> UvBounds -> Quantity units
surfaceSize f uvBounds = do
  let p00 = SurfaceFunction3d.evaluate f (Bounds2d.lowerLeftCorner uvBounds)
  let p10 = SurfaceFunction3d.evaluate f (Bounds2d.lowerRightCorner uvBounds)
  let p01 = SurfaceFunction3d.evaluate f (Bounds2d.upperLeftCorner uvBounds)
  let p11 = SurfaceFunction3d.evaluate f (Bounds2d.upperRightCorner uvBounds)
  let d1## = Point3d.distanceFrom## p00 p10
  let d2## = Point3d.distanceFrom## p10 p11
  let d3## = Point3d.distanceFrom## p11 p01
  let d4## = Point3d.distanceFrom## p01 p00
  let d5## = Point3d.distanceFrom## p00 p11
  let d6## = Point3d.distanceFrom## p10 p01
  Quantity## (max## (max## (max## (max## (max## d1## d2##) d3##) d4##) d5##) d6##)

surfaceError :: SurfaceFunction3d (space @ units) -> UvBounds -> Quantity units
surfaceError f uvBounds = do
  let fuuBounds = VectorSurfaceFunction3d.evaluateBounds f.du.du uvBounds
  let fuvBounds = VectorSurfaceFunction3d.evaluateBounds f.du.dv uvBounds
  let fvvBounds = VectorSurfaceFunction3d.evaluateBounds f.dv.dv uvBounds
  SurfaceLinearization.error fuuBounds fuvBounds fvvBounds uvBounds

addBoundaryInnerEdgeVertices ::
  Resolution units ->
  Map SurfaceId (Set2d UvBounds UvCoordinates) ->
  BoundarySurface (space @ units) ->
  Map HalfEdgeId (List (Vertex (space @ units))) ->
  Map HalfEdgeId (List (Vertex (space @ units)))
addBoundaryInnerEdgeVertices resolution surfaceSegmentsById boundarySurface accumulated = do
  let BoundarySurface{edgeLoops} = boundarySurface
  NonEmpty.foldr (addLoopInnerEdgeVertices resolution surfaceSegmentsById) accumulated edgeLoops

addLoopInnerEdgeVertices ::
  Resolution units ->
  Map SurfaceId (Set2d UvBounds UvCoordinates) ->
  NonEmpty (Edge (space @ units)) ->
  Map HalfEdgeId (List (Vertex (space @ units))) ->
  Map HalfEdgeId (List (Vertex (space @ units)))
addLoopInnerEdgeVertices resolution surfaceSegmentsById loop accumulated =
  NonEmpty.foldr (addInnerEdgeVertices resolution surfaceSegmentsById) accumulated loop

addInnerEdgeVertices ::
  Resolution units ->
  Map SurfaceId (Set2d UvBounds UvCoordinates) ->
  Edge (space @ units) ->
  Map HalfEdgeId (List (Vertex (space @ units))) ->
  Map HalfEdgeId (List (Vertex (space @ units)))
addInnerEdgeVertices resolution surfaceSegmentsById edge accumulated = do
  case edge of
    DegenerateEdge{halfEdgeId, uvCurve, point} -> do
      let HalfEdgeId{surfaceId} = halfEdgeId
      case Map.get surfaceId surfaceSegmentsById of
        Just surfaceSegments -> do
          let edgePredicate = degenerateEdgeLinearizationPredicate uvCurve surfaceSegments
          let tValues = Domain1d.innerSamplingPoints edgePredicate
          let vertex tValue = Vertex (Curve2d.evaluate uvCurve tValue) point
          Map.set halfEdgeId (List.map vertex tValues) accumulated
        Nothing ->
          throw (InternalError "Should always be able to look up surface segments for a given edge")
    PrimaryEdge{halfEdgeId, matingId, curve3d, uvCurve, matingUvCurve, correctlyAligned} -> do
      let HalfEdgeId{surfaceId} = halfEdgeId
      let HalfEdgeId{surfaceId = matingSurfaceId} = matingId
      case (Map.get surfaceId surfaceSegmentsById, Map.get matingSurfaceId surfaceSegmentsById) of
        (Just surfaceSegments, Just matingSurfaceSegments) -> do
          let edgePredicate =
                edgeLinearizationPredicate
                  resolution
                  curve3d
                  uvCurve
                  matingUvCurve
                  correctlyAligned
                  surfaceSegments
                  matingSurfaceSegments
                  curve3d.derivative.derivative
          let tValues = Domain1d.innerSamplingPoints edgePredicate
          let vertexPair tValue = do
                let point = Curve3d.evaluate curve3d tValue
                let uvPoint = Curve2d.evaluate uvCurve tValue
                let matingTValue = if correctlyAligned then 1 -. tValue else tValue
                let matingUvPoint = Curve2d.evaluate matingUvCurve matingTValue
                (Vertex uvPoint point, Vertex matingUvPoint point)
          let (vertices, matingVertices) = List.unzip2 (List.map vertexPair tValues)
          let alignedMatingVertices =
                if correctlyAligned then List.reverse matingVertices else matingVertices
          accumulated
            & Map.set halfEdgeId vertices
            & Map.set matingId alignedMatingVertices
        _ ->
          throw (InternalError "Should always be able to look up surface segments for a given edge")
    SecondaryEdge{} -> accumulated

edgeLinearizationPredicate ::
  Resolution units ->
  Curve3d (space @ units) ->
  Curve2d UvCoordinates ->
  Curve2d UvCoordinates ->
  Bool ->
  Set2d UvBounds UvCoordinates ->
  Set2d UvBounds UvCoordinates ->
  VectorCurve3d (space @ units) ->
  Bounds Unitless ->
  Bool
edgeLinearizationPredicate
  resolution
  curve3d
  uvCurve
  matingUvCurve
  correctlyAligned
  surfaceSegments
  matingSurfaceSegments
  edgeSecondDerivative
  tBounds = do
    let Bounds tStart tEnd = tBounds
    let uvStart = Curve2d.evaluate uvCurve tStart
    let uvEnd = Curve2d.evaluate uvCurve tEnd
    let matingTStart = if correctlyAligned then 1 -. tStart else tStart
    let matingTEnd = if correctlyAligned then 1 -. tEnd else tEnd
    let matingUvStart = Curve2d.evaluate matingUvCurve matingTStart
    let matingUvEnd = Curve2d.evaluate matingUvCurve matingTEnd
    let uvBounds = Bounds2d.hull2 uvStart uvEnd
    let matingUvBounds = Bounds2d.hull2 matingUvStart matingUvEnd
    let edgeSize = Point2d.distanceFrom uvStart uvEnd
    let matingEdgeSize = Point2d.distanceFrom matingUvStart matingUvEnd
    let startPoint = Curve3d.evaluate curve3d tStart
    let endPoint = Curve3d.evaluate curve3d tEnd
    let edgeLength = Point3d.distanceFrom startPoint endPoint
    let edgeSecondDerivativeBounds = VectorCurve3d.evaluateBounds edgeSecondDerivative tBounds
    let edgeSecondDerivativeMagnitude = VectorBounds3d.magnitude edgeSecondDerivativeBounds
    edgeLength <= resolution.maxSize
      && Linearization.error edgeSecondDerivativeMagnitude tBounds <= resolution.maxError
      && validEdge uvBounds edgeSize surfaceSegments
      && validEdge matingUvBounds matingEdgeSize matingSurfaceSegments

degenerateEdgeLinearizationPredicate ::
  Curve2d UvCoordinates ->
  Set2d UvBounds UvCoordinates ->
  Bounds Unitless ->
  Bool
degenerateEdgeLinearizationPredicate uvCurve surfaceSegments tBounds = do
  let Bounds tStart tEnd = tBounds
  let uvStart = Curve2d.evaluate uvCurve tStart
  let uvEnd = Curve2d.evaluate uvCurve tEnd
  let uvBounds = Bounds2d.hull2 uvStart uvEnd
  let edgeSize = Point2d.distanceFrom uvStart uvEnd
  validEdge uvBounds edgeSize surfaceSegments

validEdge :: UvBounds -> Number -> Set2d UvBounds UvCoordinates -> Bool
validEdge edgeBounds edgeLength surfaceSegments = Tolerance.using Quantity.zero do
  case surfaceSegments of
    Set2d.Node nodeBounds left right ->
      not (edgeBounds `intersects` nodeBounds)
        || (validEdge edgeBounds edgeLength left && validEdge edgeBounds edgeLength right)
    Set2d.Leaf leafBounds _ ->
      not (edgeBounds `intersects` leafBounds)
        || edgeLength <= Number.sqrt 2 .*. Bounds2d.diameter leafBounds

boundarySurfaceMesh ::
  Tolerance units =>
  Map SurfaceId (Set2d UvBounds UvCoordinates) ->
  Map HalfEdgeId (List (Vertex (space @ units))) ->
  BoundarySurface (space @ units) ->
  Mesh (Point3d (space @ units), Direction3d space)
boundarySurfaceMesh surfaceSegmentsById innerEdgeVerticesById boundarySurface = do
  let BoundarySurface{surfaceId, surfaceFunction, handedness, edgeLoops} = boundarySurface
  case SurfaceFunction3d.normalDirection surfaceFunction of
    Failure SurfaceFunction3d.IsDegenerate -> Mesh.empty
    Success normalDirection -> do
      let boundaryPolygons = NonEmpty.map (toPolygon innerEdgeVerticesById) edgeLoops
      let boundarySegments = NonEmpty.combine Polygon2d.edges boundaryPolygons
      let boundarySegmentSet = Set2d.fromNonEmpty boundarySegments
      case Map.get surfaceId surfaceSegmentsById of
        Nothing -> throw (InternalError "Should always be able to look up surface segments by ID")
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
                    List.filterMap (steinerPoint boundarySegmentSet) (Set2d.toList surfaceSegments)
          let steinerVertex uvPoint =
                Vertex uvPoint (SurfaceFunction3d.evaluate surfaceFunction uvPoint)
          let steinerVertices = List.map steinerVertex steinerPoints
          let boundaryVertexLoops = NonEmpty.map (.vertices) boundaryPolygons
          -- Decent refinement option: (Just (List.length steinerPoints, steinerVertex))
          let vertexMesh = CDT.unsafe boundaryVertexLoops steinerVertices
          let pointsAndNormals =
                Array.map (pointAndNormal normalDirection handedness) (Mesh.vertices vertexMesh)
          let faceIndices =
                case handedness of
                  Positive -> Mesh.faceIndices vertexMesh
                  Negative -> List.map (\(i, j, k) -> (k, j, i)) (Mesh.faceIndices vertexMesh)
          Mesh.indexed pointsAndNormals faceIndices

pointAndNormal ::
  DirectionSurfaceFunction3d space ->
  Sign ->
  Vertex (space @ units) ->
  (Point3d (space @ units), Direction3d space)
pointAndNormal n handedness (Vertex uvPoint point) =
  (point, handedness .*. DirectionSurfaceFunction3d.evaluate n uvPoint)

toPolygon ::
  Map HalfEdgeId (List (Vertex (space @ units))) ->
  NonEmpty (Edge (space @ units)) ->
  Polygon2d (Vertex (space @ units))
toPolygon innerEdgeVerticesById loop =
  Polygon2d (NonEmpty.combine (leadingEdgeVertices innerEdgeVerticesById) loop)

leadingEdgeVertices ::
  Map HalfEdgeId (List (Vertex (space @ units))) ->
  Edge (space @ units) ->
  NonEmpty (Vertex (space @ units))
leadingEdgeVertices innerEdgeVerticesById edge = case edge of
  DegenerateEdge{halfEdgeId, uvCurve, point} ->
    leadingEdgeVerticesImpl innerEdgeVerticesById halfEdgeId point uvCurve.startPoint
  PrimaryEdge{halfEdgeId, uvCurve, startPoint} ->
    leadingEdgeVerticesImpl innerEdgeVerticesById halfEdgeId startPoint uvCurve.startPoint
  SecondaryEdge{halfEdgeId, uvStartPoint, startPoint} ->
    leadingEdgeVerticesImpl innerEdgeVerticesById halfEdgeId startPoint uvStartPoint

leadingEdgeVerticesImpl ::
  Map HalfEdgeId (List (Vertex (space @ units))) ->
  HalfEdgeId ->
  Point3d (space @ units) ->
  UvPoint ->
  NonEmpty (Vertex (space @ units))
leadingEdgeVerticesImpl innerEdgeVerticesById edgeId startPoint uvStartPoint =
  case Map.get edgeId innerEdgeVerticesById of
    Just innerEdgeVertices -> Vertex uvStartPoint startPoint :| innerEdgeVertices
    Nothing -> throw (InternalError "Should always be able to look up internal edge vertices by ID")

steinerPoint ::
  Set2d (LineSegment2d (Vertex (space @ units))) UvCoordinates ->
  UvBounds ->
  Maybe UvPoint
steinerPoint boundarySegmentSet uvBounds = do
  let Bounds2d uBounds vBounds = uvBounds
  let uvPoint = Point2d (Bounds.midpoint uBounds) (Bounds.midpoint vBounds)
  if isValidSteinerPoint boundarySegmentSet uvPoint then Just uvPoint else Nothing

isValidSteinerPoint ::
  Set2d (LineSegment2d (Vertex (space @ units))) UvCoordinates ->
  UvPoint ->
  Bool
isValidSteinerPoint edgeSet uvPoint = case edgeSet of
  Set2d.Node nodeBounds left right ->
    case Bounds2d.exclusion## uvPoint nodeBounds >=## 0.5## *## Bounds2d.diameter## nodeBounds of
      1# -> True
      _ -> isValidSteinerPoint left uvPoint && isValidSteinerPoint right uvPoint
  Set2d.Leaf _ edge ->
    case LineSegment2d.distanceTo## uvPoint edge >=## 0.5## *## LineSegment2d.length## edge of
      1# -> True
      _ -> False

surfaces :: Body3d (space @ units) -> NonEmpty (Surface3d (space @ units))
surfaces (Body3d boundarySurfaces) = NonEmpty.map (.orientedSurface) boundarySurfaces

-- | Convert a body defined in local coordinates to one defined in global coordinates.
placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Body3d (local @ units) ->
  Body3d (global @ units)
placeIn frame (Body3d boundarySurfaces) =
  Body3d (NonEmpty.map (placeBoundarySurfaceIn frame) boundarySurfaces)

placeBoundarySurfaceIn ::
  Frame3d (global @ units) (Defines local) ->
  BoundarySurface (local @ units) ->
  BoundarySurface (global @ units)
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
    , orientedSurface = Surface3d.placeIn frame orientedSurface
    , surfaceFunction = SurfaceFunction3d.placeIn frame surfaceFunction
    , handedness
    , uvBounds
    , edgeLoops = NonEmpty.map (NonEmpty.map (placeEdgeIn frame)) edgeLoops
    }

placeEdgeIn ::
  Frame3d (global @ units) (Defines local) ->
  Edge (local @ units) ->
  Edge (global @ units)
placeEdgeIn frame edge = case edge of
  PrimaryEdge
    { halfEdgeId
    , startPoint
    , uvCurve
    , curve3d
    , matingId
    , matingUvCurve
    , correctlyAligned
    } ->
      PrimaryEdge
        { halfEdgeId
        , startPoint = Point3d.placeIn frame startPoint
        , uvCurve
        , curve3d = Curve3d.placeIn frame curve3d
        , matingId
        , matingUvCurve
        , correctlyAligned
        }
  SecondaryEdge{halfEdgeId, startPoint, uvStartPoint} ->
    SecondaryEdge{halfEdgeId, startPoint = Point3d.placeIn frame startPoint, uvStartPoint}
  DegenerateEdge{halfEdgeId, point, uvCurve} ->
    DegenerateEdge{halfEdgeId, point = Point3d.placeIn frame point, uvCurve}

-- | Convert a body defined in global coordinates to one defined in local coordinates.
relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Body3d (global @ units) ->
  Body3d (local @ units)
relativeTo frame body = placeIn (Frame3d.inverse frame) body
