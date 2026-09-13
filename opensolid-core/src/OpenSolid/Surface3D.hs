module OpenSolid.Surface3D
  ( Surface3D
  , Boundary
  , function
  , domain
  , outerBoundary
  , outerLoop
  , innerBoundaries
  , innerLoops
  , boundaries
  , boundaryLoops
  , vertices
  , edges
  , parametric
  , on
  , extruded
  , translational
  , ruled
  , revolved
  , nondegenerate
  , bounds
  , boundaryCurves
  , flip
  , placeIn
  , relativeTo
  , findPoint
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Axis2D (Axis2D)
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Bag qualified as Bag
import OpenSolid.Bag3D (Bag3D)
import OpenSolid.Bag3D qualified as Bag3D
import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Curve3D (Curve3D)
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.IsDegenerate (IsDegenerate (IsDegenerate))
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Plane3D qualified as Plane3D
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Region2D (Region2D)
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Region2D.Boundary qualified as Region2D.Boundary
import OpenSolid.Result qualified as Result
import OpenSolid.Set qualified as Set
import OpenSolid.Set3D (Set3D)
import OpenSolid.Set3D qualified as Set3D
import {-# SOURCE #-} OpenSolid.Surface3D.Nondegenerate qualified as Surface3D.Nondegenerate
import OpenSolid.SurfaceCurve3D (SurfaceCurve3D)
import OpenSolid.SurfaceCurve3D qualified as SurfaceCurve3D
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.SurfaceFunction2D qualified as SurfaceFunction2D
import OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.SurfacePoint3D (SurfacePoint3D)
import OpenSolid.SurfacePoint3D qualified as SurfacePoint3D
import OpenSolid.UvCurve qualified as UvCurve
import OpenSolid.UvRegion (UvRegion)
import OpenSolid.UvRegion qualified as UvRegion
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.VectorCurve3D (VectorCurve3D)

data Surface3D space = Surface3D
  { function :: SurfaceFunction3D space
  , domain :: UvRegion
  , outerBoundary :: ~(Boundary space)
  , innerBoundaries :: ~(Bag3D space (Boundary space))
  , boundaries :: ~(Set3D space (Boundary space))
  }

instance space1 ~ space2 => Bounded (Surface3D space1) (Bounds3D space2) where
  {-# INLINE bounds #-}
  bounds = bounds

type Boundary space = Set3D space (SurfaceCurve3D space)

function :: Surface3D space -> SurfaceFunction3D space
function = (.function)

domain :: Surface3D space -> UvRegion
domain = (.domain)

outerBoundary :: Surface3D space -> Boundary space
outerBoundary = (.outerBoundary)

outerLoop :: Surface3D space -> NonEmpty (SurfaceCurve3D space)
outerLoop = Set3D.toNonEmpty . outerBoundary

innerBoundaries :: Surface3D space -> Bag3D space (Boundary space)
innerBoundaries = (.innerBoundaries)

innerLoops :: Surface3D space -> List (NonEmpty (SurfaceCurve3D space))
innerLoops surface = Bag3D.toListOf Set3D.toNonEmpty (innerBoundaries surface)

boundaries :: Surface3D space -> Set3D space (Boundary space)
boundaries = (.boundaries)

boundaryLoops :: Surface3D space -> NonEmpty (NonEmpty (SurfaceCurve3D space))
boundaryLoops surface = outerLoop surface :| innerLoops surface

vertices :: Tolerance Meters => Surface3D space -> Bag3D space (SurfacePoint3D space)
vertices surface = do
  let surfaceCurves = Set3D.flatten (boundaries surface)
  let toPole surfaceCurve = case SurfaceCurve3D.nondegenerate surfaceCurve of
        Ok{} -> Nothing
        Err (IsDegenerate surfacePoint) -> case surfacePoint of
          SurfacePoint3D.Point{} -> Nothing
          SurfacePoint3D.Pole{} -> Just surfacePoint
  let poles = Set3D.filterMapItems toPole surfaceCurves
  let startPoint surfaceCurve = do
        let uvPoint = UvCurve.startPoint (SurfaceCurve3D.uvCurve surfaceCurve)
        let point = Curve3D.startPoint (SurfaceCurve3D.curve surfaceCurve)
        SurfacePoint3D.Point uvPoint point
  let startPoints = Set3D.map startPoint surfaceCurves
  let nonPole surfacePoint = not (surfacePoint ^ poles)
  let nonPoles = Set3D.filterItems nonPole startPoints
  poles <> nonPoles

edges :: Tolerance Meters => Surface3D space -> Bag3D space (Nondegenerate (SurfaceCurve3D space))
edges surface = do
  let surfaceCurves = Set3D.flatten (boundaries surface)
  let toEdge surfaceCurve = SurfaceCurve3D.nondegenerate surfaceCurve ?? Nothing
  Set3D.filterMapItems toEdge surfaceCurves

parametric :: SurfaceFunction3D space -> UvRegion -> Surface3D space
parametric givenFunction givenDomain = do
  let surfaceBoundary domainBoundary =
        Region2D.Boundary.curves domainBoundary
          & Set.map (SurfaceCurve3D.new givenFunction)
  let surfaceOuterBoundary = surfaceBoundary (Region2D.outerBoundary givenDomain)
  let surfaceInnerBoundaries = Bag.map surfaceBoundary (Region2D.innerBoundaries givenDomain)
  let surfaceBoundaries = Set3D.extend (Set3D.leaf surfaceOuterBoundary) surfaceInnerBoundaries
  Surface3D
    { function = givenFunction
    , domain = givenDomain
    , outerBoundary = surfaceOuterBoundary
    , innerBoundaries = surfaceInnerBoundaries
    , boundaries = surfaceBoundaries
    }

on :: Plane3D space -> Region2D Meters -> Surface3D space
on plane region = do
  let regionBounds = Region2D.bounds region
  let (width, height) = Bounds2D.dimensions regionBounds
  let centerPoint = Bounds2D.centerPoint regionBounds
  let centerFrame = Frame2D.atPoint centerPoint
  let regionSize = max width height
  let centeredRegion = Region2D.relativeTo centerFrame region
  let normalizedRegion = Region2D.convert (1.0 ?/? regionSize) centeredRegion
  let p0 = Point2D.placeOn plane centerPoint
  let vx = regionSize * Plane3D.xDirection plane
  let vy = regionSize * Plane3D.yDirection plane
  let planeFunction = p0 + SurfaceFunction1D.u * vx + SurfaceFunction1D.v * vy
  parametric planeFunction normalizedRegion

extruded :: Curve3D space -> Vector3D Meters space -> Surface3D space
extruded curve displacement = translational curve (displacement * Curve1D.t)

translational :: Curve3D space -> VectorCurve3D Meters space -> Surface3D space
translational uCurve vCurve =
  parametric (uCurve . SurfaceFunction1D.u + vCurve . SurfaceFunction1D.v) UvRegion.unitSquare

ruled :: Curve3D space -> Curve3D space -> Surface3D space
ruled bottom top = do
  let f1 = bottom . SurfaceFunction1D.u
  let f2 = top . SurfaceFunction1D.u
  parametric (f1 + SurfaceFunction1D.v * (f2 - f1)) UvRegion.unitSquare

revolved ::
  Tolerance Meters =>
  Plane3D space ->
  Curve2D Meters ->
  Axis2D Meters ->
  Angle ->
  Surface3D space
revolved plane curve axis angle = do
  let frame2D = Frame2D.fromYAxis axis
  let localCurve = Curve2D.relativeTo frame2D curve
  let (xCoordinate, yCoordinate) = Curve2D.coordinates localCurve
  let frame3D = Frame3D.fromBackPlane (Frame2D.placeOn plane frame2D)
  let radius = xCoordinate . SurfaceFunction1D.u
  let height = yCoordinate . SurfaceFunction1D.u
  let theta = angle * SurfaceFunction1D.v
  let surfaceFunction =
        frame3D.originPoint
          + radius * SurfaceFunction1D.cos theta * Frame3D.rightwardDirection frame3D
          + radius * SurfaceFunction1D.sin theta * Frame3D.forwardDirection frame3D
          + height * Frame3D.upwardDirection frame3D
  parametric surfaceFunction UvRegion.unitSquare

nondegenerate ::
  Tolerance Meters =>
  Surface3D space ->
  Result (IsDegenerate ()) (Nondegenerate (Surface3D space))
nondegenerate surface =
  Result.map (\_ -> Nondegenerate surface) $
    SurfaceFunction3D.nondegenerate (function surface)

bounds :: Surface3D space -> Bounds3D space
bounds surface = SurfaceFunction3D.range (Region2D.bounds surface.domain) surface.function

boundaryCurves :: Surface3D space -> Set3D space (SurfaceCurve3D space)
boundaryCurves = Set3D.flatten . boundaries

flip :: Surface3D space -> Surface3D space
flip surface =
  parametric
    (surface.function . SurfaceFunction2D.xy -SurfaceFunction1D.u SurfaceFunction1D.v)
    (Region2D.mirrorAcross Axis2D.y surface.domain)

-- | Convert a surface defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3D global local -> Surface3D local -> Surface3D global
placeIn frame surface =
  parametric (SurfaceFunction3D.placeIn frame surface.function) surface.domain

-- | Convert a surface defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3D global local -> Surface3D global -> Surface3D local
relativeTo frame surface =
  parametric (SurfaceFunction3D.relativeTo frame surface.function) surface.domain

findPoint ::
  Tolerance Meters =>
  Point3D space ->
  Surface3D space ->
  Result (IsDegenerate ()) (List (SurfacePoint3D space))
findPoint point surface =
  Result.map (Surface3D.Nondegenerate.findPoint point) (nondegenerate surface)
