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
import OpenSolid.Bounds2d (Bounded2d, Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Bounds3d (Bounded3d, Bounds3d (Bounds3d))
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.ConstrainedDelaunayTriangulation qualified as CDT
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Curve3d (Curve3d)
import OpenSolid.Curve3d qualified as Curve3d
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Domain1d qualified as Domain1d
import OpenSolid.Error qualified as Error
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Float qualified as Float
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Labels
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
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Polygon2d (Polygon2d (Polygon2d))
import OpenSolid.Polygon2d qualified as Polygon2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range (Range))
import OpenSolid.Range qualified as Range
import OpenSolid.Region2d (Region2d)
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Result qualified as Result
import OpenSolid.Set2d (Set2d)
import OpenSolid.Set2d qualified as Set2d
import OpenSolid.Set3d (Set3d)
import OpenSolid.Set3d qualified as Set3d
import OpenSolid.Surface3d (Surface3d)
import OpenSolid.Surface3d qualified as Surface3d
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction3d (SurfaceFunction3d)
import OpenSolid.SurfaceFunction3d qualified as SurfaceFunction3d
import OpenSolid.SurfaceLinearization qualified as SurfaceLinearization
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V), UvBounds, UvCoordinates, UvPoint)
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units (Meters)
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.VectorBounds3d qualified as VectorBounds3d
import OpenSolid.VectorCurve3d (VectorCurve3d)
import OpenSolid.VectorCurve3d qualified as VectorCurve3d
import OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)
import OpenSolid.VectorSurfaceFunction3d qualified as VectorSurfaceFunction3d
import OpenSolid.Vertex2d as Vertex2d (Vertex2d)
import OpenSolid.Vertex2d qualified as Vertex2d
import OpenSolid.Vertex3d (Vertex3d)
import OpenSolid.Vertex3d qualified as Vertex3d

-- | A solid body in 3D, defined by a set of boundary surfaces.
newtype Body3d (coordinateSystem :: CoordinateSystem)
  = Body3d (NonEmpty (BoundarySurface coordinateSystem))

instance FFI (Body3d (space @ Meters)) where
  representation = FFI.classRepresentation "Body3d"

data BoundarySurface (coordinateSystem :: CoordinateSystem) where
  BoundarySurface ::
    { id :: SurfaceId
    , surfaceFunctions :: SurfaceFunctions (space @ units)
    , handedness :: Sign
    , uvBounds :: UvBounds
    , edgeLoops :: NonEmpty (NonEmpty (Edge (space @ units)))
    } ->
    BoundarySurface (space @ units)

newtype SurfaceId = SurfaceId Int deriving (Eq, Ord, Show)

data SurfaceFunctions (coordinateSystem :: CoordinateSystem) where
  SurfaceFunctions ::
    { f :: SurfaceFunction3d (space @ units)
    , fu :: VectorSurfaceFunction3d (space @ units)
    , fv :: VectorSurfaceFunction3d (space @ units)
    , fuu :: VectorSurfaceFunction3d (space @ units)
    , fuv :: VectorSurfaceFunction3d (space @ units)
    , fvv :: VectorSurfaceFunction3d (space @ units)
    } ->
    SurfaceFunctions (space @ units)

data Edge (coordinateSystem :: CoordinateSystem) where
  PrimaryEdge ::
    { id :: HalfEdgeId
    , startPoint :: Point3d (space @ units)
    , uvCurve :: Curve2d UvCoordinates
    , curve3d :: Curve3d (space @ units)
    , matingId :: HalfEdgeId
    , matingUvCurve :: Curve2d UvCoordinates
    , correctlyAligned :: Bool
    } ->
    Edge (space @ units)
  SecondaryEdge ::
    { id :: HalfEdgeId
    , startPoint :: Point3d (space @ units)
    , uvStartPoint :: UvPoint
    } ->
    Edge (space @ units)
  DegenerateEdge ::
    { id :: HalfEdgeId
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
    { id :: SurfaceId
    , surface :: Surface3d (space @ units)
    , handedness :: Sign
    , halfEdgeLoops :: NonEmpty (NonEmpty (HalfEdge (space @ units)))
    } ->
    SurfaceWithHalfEdges (space @ units)

data HalfEdge (coordinateSystem :: CoordinateSystem) where
  HalfEdge ::
    { id :: HalfEdgeId
    , uvCurve :: Curve2d UvCoordinates -- UV curve parameterized by 3D arc length
    , curve3d :: Curve3d (space @ units) -- Arc length parameterized 3D curve
    , length :: Qty units -- Arc length of 3D curve
    , bounds :: Bounds3d (space @ units) -- Bounds on 3D curve
    } ->
    HalfEdge (space @ units)
  DegenerateHalfEdge ::
    { id :: HalfEdgeId
    , uvCurve :: Curve2d UvCoordinates
    , point :: Point3d (space @ units)
    } ->
    HalfEdge (space @ units)

instance Bounded3d (HalfEdge (space @ units)) (space @ units) where
  bounds HalfEdge{bounds} = bounds
  bounds DegenerateHalfEdge{point} = Bounds3d.constant point

data MatingEdge = MatingEdge
  { id :: HalfEdgeId
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
(the width, height or depth is zero).
-}
block :: Tolerance units => Bounds3d (space @ units) -> Result EmptyBody (Body3d (space @ units))
block (Bounds3d xRange yRange zRange) =
  case Region2d.rectangle (Bounds2d xRange yRange) of
    Failure Region2d.EmptyRegion -> Failure EmptyBody
    Success profile ->
      if Range.width zRange ~= Qty.zero
        then Failure EmptyBody
        else case extruded Plane3d.xy profile zRange of
          Success body -> Success body
          Failure _ -> internalError "Constructing block body from non-empty bounds should not fail"

{-| Create a sphere with the given center point and diameter.

Fails if the diameter is zero.
-}
sphere ::
  Tolerance units =>
  CenterPoint (Point3d (space @ units)) ->
  Diameter (Qty units) ->
  Result EmptyBody (Body3d (space @ units))
sphere (CenterPoint centerPoint) (Diameter diameter) =
  if diameter ~= Qty.zero
    then Failure EmptyBody
    else do
      let axis = Axis3d centerPoint Direction3d.z
      let sketchPlane = Plane3d.fromYAxis axis
      let radius = 0.5 * diameter
      let p1 = Point2d.y -radius
      let p2 = Point2d.y radius
      let profileCurves = [Curve2d.arc p1 p2 Angle.pi, Curve2d.line p2 p1]
      case Region2d.boundedBy profileCurves of
        Failure _ -> internalError "Semicircle profile construction should always succeed"
        Success profile ->
          case revolved sketchPlane profile Axis2d.y Angle.twoPi of
            Success body -> Success body
            Failure _ -> internalError "Constructing sphere from non-zero radius should not fail"

{-| Create a cylindrical body from a start point, end point and diameter.

Fails if the cylinder length or diameter is zero.
-}
cylinder ::
  Tolerance units =>
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Diameter (Qty units) ->
  Result EmptyBody (Body3d (space @ units))
cylinder startPoint endPoint (Diameter diameter) =
  case Vector3d.magnitudeAndDirection (endPoint - startPoint) of
    Failure Vector3d.IsZero -> Failure EmptyBody
    Success (length, direction) ->
      cylinderAlong (Axis3d startPoint direction) (Range Qty.zero length) (Diameter diameter)

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
  Range units ->
  Diameter (Qty units) ->
  Result EmptyBody (Body3d (space @ units))
cylinderAlong axis distance (Diameter diameter) = do
  case Region2d.circle (CenterPoint Point2d.origin) (Diameter diameter) of
    Failure Region2d.EmptyRegion -> Failure EmptyBody
    Success profile ->
      if Range.width distance ~= Qty.zero
        then Failure EmptyBody
        else case extruded (Axis3d.normalPlane axis) profile distance of
          Success body -> Success body
          Failure _ -> internalError "Constructing non-empty cylinder body should not fail"

-- | Create an extruded body from a sketch plane and profile.
extruded ::
  Tolerance units =>
  Plane3d (space @ units) (Defines local) ->
  Region2d (local @ units) ->
  Range units ->
  Result BoundedBy.Error (Body3d (space @ units))
extruded sketchPlane profile (Range d1 d2) = do
  let normal = Plane3d.normalDirection sketchPlane
  let v1 = d1 * normal
  let v2 = d2 * normal
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
  let startCap = Surface3d.planar startPlane profile
  let endCap = Surface3d.planar endPlane profile
  let sideSurface curve = Surface3d.translational (Curve2d.placeOn sketchPlane curve) displacement
  let sideSurfaces = List.map sideSurface (NonEmpty.toList (Region2d.boundaryCurves profile))
  let initialDerivative = VectorCurve3d.startValue (VectorCurve3d.derivative displacement)
  case Qty.sign (initialDerivative `dot` Plane3d.normalDirection sketchPlane) of
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
revolved sketchPlane profile axis givenAngle = do
  let frame2d = Frame2d.fromYAxis axis
  let frame3d = Frame3d.fromXzPlane (Frame2d.placeOn sketchPlane frame2d)
  let axis3d = Frame3d.zAxis frame3d
  let endPlane = Plane3d.rotateAround axis3d givenAngle sketchPlane
  let startCap = Surface3d.planar sketchPlane profile
  let endCap = Surface3d.planar endPlane profile
  let testPoint = Curve2d.startPoint (NonEmpty.first (Region2d.outerLoop profile))
  let testPointSign = Qty.sign (Point2d.signedDistanceFrom axis testPoint)
  let localProfile = Region2d.relativeTo frame2d profile
  let maxRadius = Range.maxAbs (Bounds2d.xCoordinate (Region2d.bounds localProfile))
  let angleTolerance = Angle.radians (?tolerance / maxRadius)
  let isFullRevolution = Tolerance.using angleTolerance (Qty.abs givenAngle ~= Angle.fullTurn)
  let normalizedAngle = if isFullRevolution then testPointSign * Angle.fullTurn else givenAngle
  let sideSurface localCurve = do
        let r = Curve2d.xCoordinate localCurve . SurfaceFunction.u
        let h = Curve2d.yCoordinate localCurve . SurfaceFunction.u
        let theta = SurfaceFunction.v * normalizedAngle
        let xLocal = r * SurfaceFunction.cos theta
        let yLocal = r * SurfaceFunction.sin theta
        let zLocal = h
        let localFunction = SurfaceFunction3d.xyz xLocal yLocal zLocal
        let globalFunction = SurfaceFunction3d.placeIn frame3d localFunction
        Surface3d.parametric globalFunction Region2d.unit
  let localProfileCurves = NonEmpty.toList (Region2d.boundaryCurves localProfile)
  let isOnAxis curve = Curve2d.xCoordinate curve ~= Qty.zero
  let revolvedCurves = List.filter (not . isOnAxis) localProfileCurves
  let sideSurfaces = List.map sideSurface revolvedCurves
  if isFullRevolution
    then boundedBy sideSurfaces
    else case Qty.sign givenAngle * testPointSign of
      Positive -> boundedBy (endCap : startCap : sideSurfaces)
      Negative -> boundedBy (startCap : endCap : sideSurfaces)

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
boundedBy (NonEmpty surfaces) = Result.do
  surfacesWithHalfEdges <- Result.collect toSurfaceWithHalfEdges (NonEmpty.indexed surfaces)
  let firstSurfaceWithHalfEdges = NonEmpty.first surfacesWithHalfEdges
  let halfEdges = NonEmpty.collect getAllHalfEdges surfacesWithHalfEdges
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
    ([], []) -> internalError "Should always have at least one processed surface"
    ([], NonEmpty processedSurfaces) ->
      Success (Body3d (NonEmpty.map (toBoundarySurface edges) processedSurfaces))

surfaceWithHalfEdgesMapEntry ::
  SurfaceWithHalfEdges (space @ units) ->
  (SurfaceId, SurfaceWithHalfEdges (space @ units))
surfaceWithHalfEdgesMapEntry surfaceWithHalfEdges = do
  let SurfaceWithHalfEdges{id} = surfaceWithHalfEdges
  (id, surfaceWithHalfEdges)

toSurfaceWithHalfEdges ::
  Tolerance units =>
  (Int, Surface3d (space @ units)) ->
  Result BoundedBy.Error (SurfaceWithHalfEdges (space @ units))
toSurfaceWithHalfEdges (surfaceIndex, surface) = do
  let surfaceFunction = Surface3d.function surface
  let surfaceDomain = Surface3d.domain surface
  let loops = Region2d.outerLoop surfaceDomain :| Region2d.innerLoops surfaceDomain
  let surfaceId = SurfaceId surfaceIndex
  Result.map (SurfaceWithHalfEdges surfaceId surface Positive) $
    Result.collect (loopHalfEdges surfaceId surfaceFunction) (NonEmpty.indexed loops)

loopHalfEdges ::
  Tolerance units =>
  SurfaceId ->
  SurfaceFunction3d (space @ units) ->
  (Int, NonEmpty (Curve2d UvCoordinates)) ->
  Result BoundedBy.Error (NonEmpty (HalfEdge (space @ units)))
loopHalfEdges surfaceId surfaceFunction (loopIndex, loop) =
  NonEmpty.indexed loop
    |> Result.collect (toHalfEdge surfaceId (LoopId loopIndex) surfaceFunction)

toHalfEdge ::
  Tolerance units =>
  SurfaceId ->
  LoopId ->
  SurfaceFunction3d (space @ units) ->
  (Int, Curve2d UvCoordinates) ->
  Result BoundedBy.Error (HalfEdge (space @ units))
toHalfEdge surfaceId loopId surfaceFunction (curveIndex, uvCurve) = do
  let curve3d = surfaceFunction . uvCurve
  let curveId = CurveId curveIndex
  let id = HalfEdgeId{surfaceId, loopId, curveId}
  case Curve3d.arcLengthParameterization curve3d of
    Success (parameterization, length) -> do
      Success
        if length == Qty.zero
          then DegenerateHalfEdge{id, uvCurve, point = Curve3d.startPoint curve3d}
          else
            HalfEdge
              { id
              , uvCurve = uvCurve . parameterization
              , curve3d = curve3d . parameterization
              , length
              , bounds = Curve3d.bounds curve3d
              }
    Failure Curve3d.HasDegeneracy ->
      Failure BoundedBy.BoundaryCurveHasDegeneracy

getAllHalfEdges :: SurfaceWithHalfEdges (space @ units) -> NonEmpty (HalfEdge (space @ units))
getAllHalfEdges SurfaceWithHalfEdges{halfEdgeLoops} = NonEmpty.concat halfEdgeLoops

halfEdgeStartPoint :: HalfEdge (space @ units) -> Corner (space @ units)
halfEdgeStartPoint halfEdge = case halfEdge of
  HalfEdge{id = HalfEdgeId{surfaceId}, curve3d} -> Corner surfaceId (Curve3d.startPoint curve3d)
  DegenerateHalfEdge{id = HalfEdgeId{surfaceId}, point} -> Corner surfaceId point

registerSurfaceWithHalfEdges ::
  Tolerance units =>
  Set3d (Corner (space @ units)) (space @ units) ->
  Set3d (HalfEdge (space @ units)) (space @ units) ->
  SurfaceRegistry (space @ units) ->
  SurfaceWithHalfEdges (space @ units) ->
  Result BoundedBy.Error (SurfaceRegistry (space @ units))
registerSurfaceWithHalfEdges cornerSet halfEdgeSet surfaceRegistry surfaceWithHalfEdges = do
  let SurfaceWithHalfEdges{id, handedness} = surfaceWithHalfEdges
  let SurfaceRegistry{unprocessed, processed, edges} = surfaceRegistry
  let updatedRegistry =
        SurfaceRegistry
          { unprocessed = unprocessed |> Map.remove id
          , processed = processed |> Map.set id surfaceWithHalfEdges
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
    DegenerateHalfEdge{id, uvCurve, point} -> Result.do
      canonicalPoint <- getCornerPoint point cornerSet
      let edge = DegenerateEdge{id, uvCurve, point = canonicalPoint}
      Success SurfaceRegistry{unprocessed, processed, edges = Map.set id edge edges}
    HalfEdge{id, uvCurve, curve3d} -> do
      let matingEdgeCandidates = Set3d.filter (Bounds3d.bounds halfEdge) halfEdgeSet
      case Maybe.collect (toMatingEdge id curve3d) matingEdgeCandidates of
        [] -> Failure BoundedBy.BoundaryHasGaps
        List.TwoOrMore -> Failure BoundedBy.BoundaryIntersectsItself
        List.One MatingEdge{id = matingId, uvCurve = matingUvCurve, correctlyAligned} -> Result.do
          let HalfEdgeId{surfaceId = matingSurfaceId} = matingId
          startPoint <- getCornerPoint (Curve3d.startPoint curve3d) cornerSet
          let edge =
                if id < matingId
                  then
                    PrimaryEdge
                      { id
                      , startPoint
                      , uvCurve
                      , curve3d
                      , matingId
                      , matingUvCurve
                      , correctlyAligned
                      }
                  else SecondaryEdge{id, startPoint, uvStartPoint = Curve2d.startPoint uvCurve}
          let updatedRegistry =
                SurfaceRegistry{unprocessed, processed, edges = edges |> Map.set id edge}
          let matingHandedness = if correctlyAligned then parentHandedness else -parentHandedness
          case (Map.get matingSurfaceId unprocessed, Map.get matingSurfaceId processed) of
            (Nothing, Nothing) -> internalError "No surface found for half-edge"
            (Just _, Just _) -> internalError "Multiple surfaces found for half-edge"
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
setHandedness handedness SurfaceWithHalfEdges{id, surface, halfEdgeLoops} =
  SurfaceWithHalfEdges{handedness, id, surface, halfEdgeLoops}

toBoundarySurface ::
  Map HalfEdgeId (Edge (space @ units)) ->
  SurfaceWithHalfEdges (space @ units) ->
  BoundarySurface (space @ units)
toBoundarySurface edges SurfaceWithHalfEdges{id, surface, handedness, halfEdgeLoops} =
  BoundarySurface
    { id
    , surfaceFunctions = toSurfaceFunctions (Surface3d.function surface)
    , handedness
    , uvBounds = Region2d.bounds (Surface3d.domain surface)
    , edgeLoops = NonEmpty.map (NonEmpty.map (toEdge edges)) halfEdgeLoops
    }

toEdge :: Map HalfEdgeId (Edge (space @ units)) -> HalfEdge (space @ units) -> Edge (space @ units)
toEdge edges halfEdge =
  case Map.get (halfEdgeId halfEdge) edges of
    Just edge -> edge
    Nothing -> internalError "Should always be able to find edge for processed half-edge"

halfEdgeId :: HalfEdge (space @ units) -> HalfEdgeId
halfEdgeId HalfEdge{id} = id
halfEdgeId DegenerateHalfEdge{id} = id

toSurfaceFunctions :: SurfaceFunction3d (space @ units) -> SurfaceFunctions (space @ units)
toSurfaceFunctions f = do
  let fu = SurfaceFunction3d.derivative U f
  let fv = SurfaceFunction3d.derivative V f
  let fuu = VectorSurfaceFunction3d.derivative U fu
  let fuv = VectorSurfaceFunction3d.derivative V fu
  let fvv = VectorSurfaceFunction3d.derivative V fv
  SurfaceFunctions{f, fu, fv, fuu, fuv, fvv}

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
    [] -> internalError "getCorner should always find at least one corner (the given point itself)"
    NonEmpty candidates -> Success (cornerPoint (NonEmpty.minimumBy cornerSurfaceId candidates))

toMatingEdge ::
  Tolerance units =>
  HalfEdgeId ->
  Curve3d (space @ units) ->
  HalfEdge (space @ units) ->
  Maybe MatingEdge
toMatingEdge _ _ DegenerateHalfEdge{} = Nothing
toMatingEdge id1 curve1 HalfEdge{id = id2, curve3d = curve2, uvCurve}
  | id1 == id2 = Nothing
  | otherwise = do
      let points1 = List.map (Curve3d.evaluate curve1) Parameter.samples
      let points2 = List.map (Curve3d.evaluate curve2) Parameter.samples
      if
        | points1 ~= points2 -> Just MatingEdge{id = id2, uvCurve, correctlyAligned = False}
        | points1 ~= List.reverse points2 -> Just MatingEdge{id = id2, uvCurve, correctlyAligned = True}
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
  NonEmpty (Mesh.Constraint units) ->
  Body3d (space @ units) ->
  Mesh (Point3d (space @ units), Vector3d (space @ Unitless))
toMesh givenConstraints (Body3d boundarySurfaces) = do
  let boundarySurfaceList = NonEmpty.toList boundarySurfaces
  let constraints = Mesh.constraints givenConstraints
  let surfaceSegmentsById = Map.fromList (boundarySurfaceSegments constraints) boundarySurfaceList
  let innerEdgeVerticesById =
        NonEmpty.foldr
          (addBoundaryInnerEdgeVertices constraints surfaceSegmentsById)
          Map.empty
          boundarySurfaces
  Mesh.collect (boundarySurfaceMesh surfaceSegmentsById innerEdgeVerticesById) boundarySurfaces

boundarySurfaceSegments ::
  Mesh.Constraints units ->
  BoundarySurface (space @ units) ->
  (SurfaceId, Set2d UvBounds UvCoordinates)
boundarySurfaceSegments constraints BoundarySurface{id, surfaceFunctions, uvBounds} =
  (id, boundarySurfaceSegmentSet constraints surfaceFunctions uvBounds)

boundarySurfaceSegmentSet ::
  Mesh.Constraints units ->
  SurfaceFunctions (space @ units) ->
  UvBounds ->
  Set2d UvBounds UvCoordinates
boundarySurfaceSegmentSet constraints surfaceFunctions uvBounds = do
  let Mesh.Constraints{maxError, maxSize} = constraints
  if surfaceSize surfaceFunctions uvBounds <= maxSize
    && surfaceError surfaceFunctions uvBounds <= maxError
    then Set2d.Leaf uvBounds uvBounds
    else do
      let Bounds2d uRange vRange = uvBounds
      let (u1, u2) = Range.bisect uRange
      let (v1, v2) = Range.bisect vRange
      let set11 = boundarySurfaceSegmentSet constraints surfaceFunctions (Bounds2d u1 v1)
      let set12 = boundarySurfaceSegmentSet constraints surfaceFunctions (Bounds2d u1 v2)
      let set21 = boundarySurfaceSegmentSet constraints surfaceFunctions (Bounds2d u2 v1)
      let set22 = boundarySurfaceSegmentSet constraints surfaceFunctions (Bounds2d u2 v2)
      let set1 = Set2d.Node (Bounds2d u1 vRange) set11 set12
      let set2 = Set2d.Node (Bounds2d u2 vRange) set21 set22
      Set2d.Node uvBounds set1 set2

surfaceSize :: SurfaceFunctions (space @ units) -> UvBounds -> Qty units
surfaceSize SurfaceFunctions{f} uvBounds = do
  let p00 = SurfaceFunction3d.evaluate f (Bounds2d.lowerLeftCorner uvBounds)
  let p10 = SurfaceFunction3d.evaluate f (Bounds2d.lowerRightCorner uvBounds)
  let p01 = SurfaceFunction3d.evaluate f (Bounds2d.upperLeftCorner uvBounds)
  let p11 = SurfaceFunction3d.evaluate f (Bounds2d.upperRightCorner uvBounds)
  Point3d.distanceFrom p00 p10
    |> Qty.max (Point3d.distanceFrom p10 p11)
    |> Qty.max (Point3d.distanceFrom p11 p01)
    |> Qty.max (Point3d.distanceFrom p01 p00)
    |> Qty.max (Point3d.distanceFrom p00 p11)
    |> Qty.max (Point3d.distanceFrom p10 p01)

surfaceError :: SurfaceFunctions (space @ units) -> UvBounds -> Qty units
surfaceError SurfaceFunctions{fuu, fuv, fvv} uvBounds = do
  let fuuBounds = VectorSurfaceFunction3d.evaluateBounds fuu uvBounds
  let fuvBounds = VectorSurfaceFunction3d.evaluateBounds fuv uvBounds
  let fvvBounds = VectorSurfaceFunction3d.evaluateBounds fvv uvBounds
  SurfaceLinearization.error fuuBounds fuvBounds fvvBounds uvBounds

addBoundaryInnerEdgeVertices ::
  Mesh.Constraints units ->
  Map SurfaceId (Set2d UvBounds UvCoordinates) ->
  BoundarySurface (space @ units) ->
  Map HalfEdgeId (List (Vertex (space @ units))) ->
  Map HalfEdgeId (List (Vertex (space @ units)))
addBoundaryInnerEdgeVertices constraints surfaceSegmentsById boundarySurface accumulated = do
  let BoundarySurface{edgeLoops} = boundarySurface
  NonEmpty.foldr (addLoopInnerEdgeVertices constraints surfaceSegmentsById) accumulated edgeLoops

addLoopInnerEdgeVertices ::
  Mesh.Constraints units ->
  Map SurfaceId (Set2d UvBounds UvCoordinates) ->
  NonEmpty (Edge (space @ units)) ->
  Map HalfEdgeId (List (Vertex (space @ units))) ->
  Map HalfEdgeId (List (Vertex (space @ units)))
addLoopInnerEdgeVertices constraints surfaceSegmentsById loop accumulated =
  NonEmpty.foldr (addInnerEdgeVertices constraints surfaceSegmentsById) accumulated loop

addInnerEdgeVertices ::
  Mesh.Constraints units ->
  Map SurfaceId (Set2d UvBounds UvCoordinates) ->
  Edge (space @ units) ->
  Map HalfEdgeId (List (Vertex (space @ units))) ->
  Map HalfEdgeId (List (Vertex (space @ units)))
addInnerEdgeVertices constraints surfaceSegmentsById edge accumulated = do
  case edge of
    DegenerateEdge{id, uvCurve, point} -> do
      let HalfEdgeId{surfaceId} = id
      case Map.get surfaceId surfaceSegmentsById of
        Just surfaceSegments -> do
          let edgePredicate = degenerateEdgeLinearizationPredicate uvCurve surfaceSegments
          let tValues = Domain1d.innerSamplingPoints edgePredicate
          let vertex tValue = Vertex (Curve2d.evaluate uvCurve tValue) point
          Map.set id (List.map vertex tValues) accumulated
        Nothing ->
          internalError "Should always be able to look up surface segments for a given edge"
    PrimaryEdge{id, matingId, curve3d, uvCurve, matingUvCurve, correctlyAligned} -> do
      let HalfEdgeId{surfaceId} = id
      let HalfEdgeId{surfaceId = matingSurfaceId} = matingId
      case (Map.get surfaceId surfaceSegmentsById, Map.get matingSurfaceId surfaceSegmentsById) of
        (Just surfaceSegments, Just matingSurfaceSegments) -> do
          let edgePredicate =
                edgeLinearizationPredicate
                  constraints
                  curve3d
                  uvCurve
                  matingUvCurve
                  correctlyAligned
                  surfaceSegments
                  matingSurfaceSegments
                  (VectorCurve3d.derivative (Curve3d.derivative curve3d))
          let tValues = Domain1d.innerSamplingPoints edgePredicate
          let vertexPair tValue = do
                let point = Curve3d.evaluate curve3d tValue
                let uvPoint = Curve2d.evaluate uvCurve tValue
                let matingTValue = if correctlyAligned then 1.0 - tValue else tValue
                let matingUvPoint = Curve2d.evaluate matingUvCurve matingTValue
                (Vertex uvPoint point, Vertex matingUvPoint point)
          let (vertices, matingVertices) = List.unzip2 (List.map vertexPair tValues)
          accumulated
            |> Map.set id vertices
            |> Map.set matingId (if correctlyAligned then List.reverse matingVertices else matingVertices)
        _ -> internalError "Should always be able to look up surface segments for a given edge"
    SecondaryEdge{} -> accumulated

edgeLinearizationPredicate ::
  Mesh.Constraints units ->
  Curve3d (space @ units) ->
  Curve2d UvCoordinates ->
  Curve2d UvCoordinates ->
  Bool ->
  Set2d UvBounds UvCoordinates ->
  Set2d UvBounds UvCoordinates ->
  VectorCurve3d (space @ units) ->
  Range Unitless ->
  Bool
edgeLinearizationPredicate
  constraints
  curve3d
  uvCurve
  matingUvCurve
  correctlyAligned
  surfaceSegments
  matingSurfaceSegments
  edgeSecondDerivative
  tRange = do
    let Range tStart tEnd = tRange
    let uvStart = Curve2d.evaluate uvCurve tStart
    let uvEnd = Curve2d.evaluate uvCurve tEnd
    let matingTStart = if correctlyAligned then 1.0 - tStart else tStart
    let matingTEnd = if correctlyAligned then 1.0 - tEnd else tEnd
    let matingUvStart = Curve2d.evaluate matingUvCurve matingTStart
    let matingUvEnd = Curve2d.evaluate matingUvCurve matingTEnd
    let uvBounds = Bounds2d.hull2 uvStart uvEnd
    let matingUvBounds = Bounds2d.hull2 matingUvStart matingUvEnd
    let edgeSize = Point2d.distanceFrom uvStart uvEnd
    let matingEdgeSize = Point2d.distanceFrom matingUvStart matingUvEnd
    let startPoint = Curve3d.evaluate curve3d tStart
    let endPoint = Curve3d.evaluate curve3d tEnd
    let edgeLength = Point3d.distanceFrom startPoint endPoint
    let edgeSecondDerivativeBounds = VectorCurve3d.evaluateBounds edgeSecondDerivative tRange
    let edgeSecondDerivativeMagnitude = VectorBounds3d.magnitude edgeSecondDerivativeBounds
    let Mesh.Constraints{maxError, maxSize} = constraints
    edgeLength <= maxSize
      && Linearization.error edgeSecondDerivativeMagnitude tRange <= maxError
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
  Map HalfEdgeId (List (Vertex (space @ units))) ->
  BoundarySurface (space @ units) ->
  Mesh (Point3d (space @ units), Vector3d (space @ Unitless))
boundarySurfaceMesh surfaceSegmentsById innerEdgeVerticesById boundarySurface = do
  let BoundarySurface{id, surfaceFunctions, handedness, edgeLoops} = boundarySurface
  let boundaryPolygons = NonEmpty.map (toPolygon innerEdgeVerticesById) edgeLoops
  let boundarySegments = NonEmpty.collect Polygon2d.edges boundaryPolygons
  let boundarySegmentSet = Set2d.fromNonEmpty boundarySegments
  case Map.get id surfaceSegmentsById of
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
      -- Decent refinement option: (Just (List.length steinerPoints, steinerVertex))
      let vertexMesh = CDT.unsafe boundaryVertexLoops steinerVertices Nothing
      let pointsAndNormals =
            Array.map (pointAndNormal surfaceFunctions handedness) (Mesh.vertices vertexMesh)
      let faceIndices =
            case handedness of
              Positive -> Mesh.faceIndices vertexMesh
              Negative -> List.map (\(i, j, k) -> (k, j, i)) (Mesh.faceIndices vertexMesh)
      Mesh.indexed pointsAndNormals faceIndices

pointAndNormal ::
  SurfaceFunctions (space @ units) ->
  Sign ->
  Vertex (space @ units) ->
  (Point3d (space @ units), Vector3d (space @ Unitless))
pointAndNormal SurfaceFunctions{fu, fv} handedness (Vertex uvPoint point) = do
  let fuValue = VectorSurfaceFunction3d.evaluate fu uvPoint
  let fvValue = VectorSurfaceFunction3d.evaluate fv uvPoint
  (point, handedness * Vector3d.normalize (fuValue `cross'` fvValue))

toPolygon ::
  Map HalfEdgeId (List (Vertex (space @ units))) ->
  NonEmpty (Edge (space @ units)) ->
  Polygon2d (Vertex (space @ units))
toPolygon innerEdgeVerticesById loop =
  Polygon2d (NonEmpty.collect (leadingEdgeVertices innerEdgeVerticesById) loop)

leadingEdgeVertices ::
  Map HalfEdgeId (List (Vertex (space @ units))) ->
  Edge (space @ units) ->
  NonEmpty (Vertex (space @ units))
leadingEdgeVertices innerEdgeVerticesById edge = case edge of
  DegenerateEdge{id, uvCurve, point} ->
    leadingEdgeVerticesImpl innerEdgeVerticesById id point (Curve2d.startPoint uvCurve)
  PrimaryEdge{id, uvCurve, startPoint} ->
    leadingEdgeVerticesImpl innerEdgeVerticesById id startPoint (Curve2d.startPoint uvCurve)
  SecondaryEdge{id, uvStartPoint, startPoint} ->
    leadingEdgeVerticesImpl innerEdgeVerticesById id startPoint uvStartPoint

leadingEdgeVerticesImpl ::
  Map HalfEdgeId (List (Vertex (space @ units))) ->
  HalfEdgeId ->
  Point3d (space @ units) ->
  UvPoint ->
  NonEmpty (Vertex (space @ units))
leadingEdgeVerticesImpl innerEdgeVerticesById edgeId startPoint uvStartPoint =
  case Map.get edgeId innerEdgeVerticesById of
    Just innerEdgeVertices -> Vertex uvStartPoint startPoint :| innerEdgeVertices
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
