module OpenSolid.Surface3D
  ( Surface3D
  , function
  , domain
  , outerLoop
  , innerLoops
  , parametric
  , on
  , extruded
  , translational
  , ruled
  , revolved
  , overallBounds
  , boundaryCurves
  , boundarySurfaceCurves
  , flip
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Axis2D (Axis2D)
import OpenSolid.Axis2D qualified as Axis2D
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
import OpenSolid.List qualified as List
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Plane3D qualified as Plane3D
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Region2D (Region2D)
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Set qualified as Set
import OpenSolid.Set3D (Set3D)
import OpenSolid.Set3D qualified as Set3D
import OpenSolid.SurfaceCurve3D (SurfaceCurve3D)
import OpenSolid.SurfaceCurve3D qualified as SurfaceCurve3D
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.SurfaceFunction2D qualified as SurfaceFunction2D
import OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.UvRegion (UvRegion)
import OpenSolid.UvRegion qualified as UvRegion
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.VectorCurve3D (VectorCurve3D)

data Surface3D space = Surface3D
  { function :: SurfaceFunction3D space
  , domain :: UvRegion
  , outerLoop :: ~(Set3D space (SurfaceCurve3D space))
  , innerLoops :: ~(List (Set3D space (SurfaceCurve3D space)))
  }

function :: Surface3D space -> SurfaceFunction3D space
function = (.function)

domain :: Surface3D space -> UvRegion
domain = (.domain)

outerLoop :: Surface3D space -> Set3D space (SurfaceCurve3D space)
outerLoop = (.outerLoop)

innerLoops :: Surface3D space -> List (Set3D space (SurfaceCurve3D space))
innerLoops = (.innerLoops)

parametric :: SurfaceFunction3D space -> UvRegion -> Surface3D space
parametric givenFunction givenDomain = do
  let boundaryLoop domainLoop =
        Set.map (SurfaceCurve3D.new givenFunction) SurfaceCurve3D.bounds domainLoop
  Surface3D
    { function = givenFunction
    , domain = givenDomain
    , outerLoop = boundaryLoop (Region2D.outerLoop givenDomain)
    , innerLoops = List.map boundaryLoop (Region2D.innerLoops givenDomain)
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

overallBounds :: Surface3D space -> Bounds3D space
overallBounds surface = SurfaceFunction3D.bounds surface.function (Region2D.bounds surface.domain)

boundaryCurves :: Surface3D space -> Set3D space (Curve3D space)
boundaryCurves surface =
  Set.map SurfaceCurve3D.curve Curve3D.overallBounds (boundarySurfaceCurves surface)

boundarySurfaceCurves :: Surface3D space -> Set3D space (SurfaceCurve3D space)
boundarySurfaceCurves surface = Set3D.aggregate (surface.outerLoop :| surface.innerLoops)

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
