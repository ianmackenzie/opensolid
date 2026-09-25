module OpenSolid.Surface3D
  ( Surface3D
  , IsDegenerate (IsDegenerate)
  , Boundary
  , function
  , domain
  , degenerateLeft
  , degenerateRight
  , degenerateTop
  , degenerateBottom
  , outerBoundary
  , outerLoop
  , innerBoundaries
  , innerLoops
  , boundaries
  , boundaryLoops
  , vertices
  , edges
  , bisectionTree
  , pointAt
  , pointOn
  , partialDerivativesAt
  , partialDerivativeRanges
  , secondPartialDerivativesAt
  , secondPartialDerivativeRanges
  , normalDirectionAt
  , normalDirectionRange
  , vertexAt
  , vertexOn
  , parametric
  , on
  , extruded
  , translational
  , ruled
  , revolved
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
import OpenSolid.Bag qualified as Bag
import OpenSolid.Bag3D (Bag3D)
import OpenSolid.Bag3D qualified as Bag3D
import OpenSolid.Bisection qualified as Bisection
import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Curve3D (Curve3D)
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.Degeneracy qualified as Degeneracy
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.IsDegenerate qualified as IsDegenerate
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Plane3D qualified as Plane3D
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Region2D (Region2D)
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Region2D.Boundary qualified as Region2D.Boundary
import OpenSolid.Set qualified as Set
import OpenSolid.Set3D (Set3D)
import OpenSolid.Set3D qualified as Set3D
import OpenSolid.Space qualified as Space
import {-# SOURCE #-} OpenSolid.Surface3D.FindPoint qualified as Surface3D.FindPoint
import OpenSolid.Surface3D.Segment (Segment (..))
import OpenSolid.SurfaceCurve3D (SurfaceCurve3D)
import OpenSolid.SurfaceCurve3D qualified as SurfaceCurve3D
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.SurfacePoint3D (SurfacePoint3D)
import OpenSolid.SurfacePoint3D qualified as SurfacePoint3D
import OpenSolid.SurfaceVertex3D (SurfaceVertex3D (SurfaceVertex3D))
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvBounds (UvBounds, data UvBounds)
import OpenSolid.UvBounds qualified as UvBounds
import OpenSolid.UvCurve qualified as UvCurve
import OpenSolid.UvPoint (UvPoint, data UvPoint)
import OpenSolid.UvPoint qualified as UvPoint
import OpenSolid.UvRegion (UvRegion)
import OpenSolid.UvRegion qualified as UvRegion
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.Vector3D.Nonzero qualified as Vector3D.Nonzero
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorBounds3D qualified as VectorBounds3D
import OpenSolid.VectorCurve3D (VectorCurve3D)

data Surface3D space = Surface3D
  { function :: SurfaceFunction3D space
  , domain :: UvRegion
  , outerBoundary :: ~(Boundary space)
  , innerBoundaries :: ~(Bag3D space (Boundary space))
  , degenerateLeft :: ~Bool
  , degenerateRight :: ~Bool
  , degenerateBottom :: ~Bool
  , degenerateTop :: ~Bool
  , bisectionTree :: ~(BisectionTree space)
  }

data IsDegenerate = IsDegenerate deriving (Eq, Show, Err)

type BisectionTree space = Bisection.Tree UvBounds (Segment space)

instance Space.Coercion (Surface3D space1) (Surface3D space2) where
  coerce surface =
    Surface3D
      { function = Space.coerce surface.function
      , domain = surface.domain
      , outerBoundary = Set3D.map Space.coerce surface.outerBoundary
      , innerBoundaries = Bag3D.map (Set3D.map Space.coerce) surface.innerBoundaries
      , degenerateLeft = surface.degenerateLeft
      , degenerateRight = surface.degenerateRight
      , degenerateBottom = surface.degenerateBottom
      , degenerateTop = surface.degenerateTop
      , bisectionTree = Space.coerce surface.bisectionTree
      }

instance space1 ~ space2 => Bounded (Surface3D space1) (Bounds3D space2) where
  {-# INLINE bounds #-}
  bounds = bounds

type Boundary space = Set3D space (SurfaceCurve3D space)

{-# INLINE function #-}
function :: Surface3D space -> SurfaceFunction3D space
function = (.function)

{-# INLINE domain #-}
domain :: Surface3D space -> UvRegion
domain = (.domain)

{-# INLINE degenerateLeft #-}
degenerateLeft :: Surface3D space -> Bool
degenerateLeft = (.degenerateLeft)

{-# INLINE degenerateRight #-}
degenerateRight :: Surface3D space -> Bool
degenerateRight = (.degenerateRight)

{-# INLINE degenerateBottom #-}
degenerateBottom :: Surface3D space -> Bool
degenerateBottom = (.degenerateBottom)

{-# INLINE degenerateTop #-}
degenerateTop :: Surface3D space -> Bool
degenerateTop = (.degenerateTop)

outerBoundary :: Surface3D space -> Boundary space
outerBoundary = (.outerBoundary)

outerLoop :: Surface3D space -> NonEmpty (SurfaceCurve3D space)
outerLoop = Set3D.toNonEmpty . outerBoundary

innerBoundaries :: Surface3D space -> Bag3D space (Boundary space)
innerBoundaries = (.innerBoundaries)

innerLoops :: Surface3D space -> List (NonEmpty (SurfaceCurve3D space))
innerLoops surface = Bag3D.toListOf Set3D.toNonEmpty (innerBoundaries surface)

boundaries :: Surface3D space -> Set3D space (Boundary space)
boundaries surface = Set3D.extend (Set3D.leaf (outerBoundary surface)) (innerBoundaries surface)

boundaryLoops :: Surface3D space -> NonEmpty (NonEmpty (SurfaceCurve3D space))
boundaryLoops surface = outerLoop surface :| innerLoops surface

vertices :: Tolerance Meters => Surface3D space -> Bag3D space (SurfacePoint3D space)
vertices surface = do
  let surfaceCurves = Set3D.flatten (boundaries surface)
  let toPole surfaceCurve = case SurfaceCurve3D.nondegenerate surfaceCurve of
        Ok{} -> Nothing
        Err (IsDegenerate.IsDegenerate surfacePoint) -> case surfacePoint of
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

bisectionTree :: Surface3D space -> BisectionTree space
bisectionTree = (.bisectionTree)

{-# INLINE pointAt #-}
pointAt :: UvPoint -> Surface3D space -> Point3D space
pointAt uvPoint surface = SurfaceFunction3D.pointAt uvPoint (function surface)

{-# INLINE pointOn #-}
pointOn :: Surface3D space -> UvPoint -> Point3D space
pointOn surface uvPoint = SurfaceFunction3D.pointOn (function surface) uvPoint

{-# INLINE range #-}
range :: UvBounds -> Surface3D space -> Bounds3D space
range uvRange surface = SurfaceFunction3D.range uvRange (function surface)

partialDerivativesAt ::
  UvPoint ->
  Surface3D space ->
  (Vector3D Meters space, Vector3D Meters space)
partialDerivativesAt uvPoint surface =
  SurfaceFunction3D.partialDerivativesAt uvPoint (function surface)

partialDerivativeRanges ::
  UvBounds ->
  Surface3D space ->
  (VectorBounds3D Meters space, VectorBounds3D Meters space)
partialDerivativeRanges uvRange surface =
  SurfaceFunction3D.partialDerivativeRanges uvRange (function surface)

secondPartialDerivativesAt ::
  UvPoint ->
  Surface3D space ->
  (Vector3D Meters space, Vector3D Meters space, Vector3D Meters space)
secondPartialDerivativesAt uvPoint surface =
  SurfaceFunction3D.secondPartialDerivativesAt uvPoint (function surface)

secondPartialDerivativeRanges ::
  UvBounds ->
  Surface3D space ->
  (VectorBounds3D Meters space, VectorBounds3D Meters space, VectorBounds3D Meters space)
secondPartialDerivativeRanges uvRange surface =
  SurfaceFunction3D.secondPartialDerivativeRanges uvRange (function surface)

normalDirectionAt :: UvPoint -> Surface3D space -> Direction3D space
normalDirectionAt uvPoint surface = do
  let UvPoint uValue vValue = uvPoint
  let (fu, fv) = partialDerivativesAt uvPoint surface
  let (fuu, fuv, fvv) = secondPartialDerivativesAt uvPoint surface
  let nu = fuu `cross` fv + fu `cross` fuv
  let nv = fuv `cross` fv + fu `cross` fvv
  Vector3D.Nonzero.direction . Nonzero $
    if
      | uValue == 0.0 && degenerateLeft surface -> nu
      | uValue == 1.0 && degenerateRight surface -> -nu
      | vValue == 0.0 && degenerateBottom surface -> nv
      | vValue == 1.0 && degenerateTop surface -> -nv
      | otherwise -> fu `cross` fv

normalDirectionRange :: UvBounds -> Surface3D space -> DirectionBounds3D space
normalDirectionRange uvRange surface = do
  let UvBounds (Interval uLow uHigh) (Interval vLow vHigh) = uvRange
  let (fu, fv) = partialDerivativeRanges uvRange surface
  let (fuu, fuv, fvv) = secondPartialDerivativeRanges uvRange surface
  let nu = fuu `cross` fv + fu `cross` fuv
  let nv = fuv `cross` fv + fu `cross` fvv
  VectorBounds3D.direction $
    if
      | uLow == 0.0 && degenerateLeft surface -> nu
      | uHigh == 1.0 && degenerateRight surface -> -nu
      | vLow == 0.0 && degenerateBottom surface -> nv
      | vHigh == 1.0 && degenerateTop surface -> -nv
      | otherwise -> fu `cross` fv

vertexAt :: UvPoint -> Surface3D space -> SurfaceVertex3D space
vertexAt uvPoint surface =
  SurfaceVertex3D (pointAt uvPoint surface) (normalDirectionAt uvPoint surface)

vertexOn :: Surface3D space -> UvPoint -> SurfaceVertex3D space
vertexOn surface uvPoint = vertexAt uvPoint surface

parametric ::
  Tolerance Meters =>
  SurfaceFunction3D space ->
  UvRegion ->
  Result IsDegenerate (Surface3D space)
parametric givenFunction givenDomain = do
  let allDegenerate samplePoints =
        NonEmpty.maximumOf (divergence givenFunction) samplePoints ~= Length.zero
  if allDegenerate UvPoint.interiorSamples
    then Err IsDegenerate
    else Ok $ recursive \surface -> do
      let (surfaceOuterBoundary, surfaceInnerBoundaries) = buildBoundaries givenFunction givenDomain
      Surface3D
        { function = givenFunction
        , domain = givenDomain
        , outerBoundary = surfaceOuterBoundary
        , innerBoundaries = surfaceInnerBoundaries
        , degenerateLeft = allDegenerate UvPoint.leftSamples
        , degenerateRight = allDegenerate UvPoint.rightSamples
        , degenerateBottom = allDegenerate UvPoint.bottomSamples
        , degenerateTop = allDegenerate UvPoint.topSamples
        , bisectionTree = buildBisectionTree UvBounds.unitSquare surface
        }

divergence :: SurfaceFunction3D space -> UvPoint -> Length
divergence surfaceFunction uvPoint = do
  let (duValue, dvValue) = SurfaceFunction3D.partialDerivativesAt uvPoint surfaceFunction
  Vector3D.divergence duValue dvValue

buildBoundaries ::
  Tolerance Meters =>
  SurfaceFunction3D space ->
  UvRegion ->
  (Boundary space, Bag3D space (Boundary space))
buildBoundaries surfaceFunction surfaceDomain = do
  let surfaceBoundary domainBoundary =
        Set.map (SurfaceCurve3D.new surfaceFunction) (Region2D.Boundary.curves domainBoundary)
  let surfaceOuterBoundary = surfaceBoundary (Region2D.outerBoundary surfaceDomain)
  let surfaceInnerBoundaries = Bag.map surfaceBoundary (Region2D.innerBoundaries surfaceDomain)
  (surfaceOuterBoundary, surfaceInnerBoundaries)

buildBisectionTree :: UvBounds -> Surface3D space -> BisectionTree space
buildBisectionTree uvRange surface = do
  let UvBounds uRange vRange = uvRange
  let (uLeft, uRight) = Interval.bisect uRange
  let (vBottom, vTop) = Interval.bisect vRange
  let bottomLeft = buildBisectionTree (UvBounds uLeft vBottom) surface
  let bottomRight = buildBisectionTree (UvBounds uRight vBottom) surface
  let topLeft = buildBisectionTree (UvBounds uLeft vTop) surface
  let topRight = buildBisectionTree (UvBounds uRight vTop) surface
  let children = NonEmpty.four bottomLeft bottomRight topLeft topRight
  Bisection.Tree uvRange (segment uvRange surface) children

segment :: UvBounds -> Surface3D space -> Segment space
segment uvRange surface = do
  let UvBounds (Interval uLow uHigh) (Interval vLow vHigh) = uvRange
  let isDegenerateLeft = uHigh <= Degeneracy.tStart && degenerateLeft surface
  let isDegenerateRight = uLow >= Degeneracy.tEnd && degenerateRight surface
  let isDegenerateBottom = vHigh <= Degeneracy.tStart && degenerateBottom surface
  let isDegenerateTop = vLow >= Degeneracy.tEnd && degenerateTop surface
  let derivativeRanges = partialDerivativeRanges uvRange surface
  let (duRange, dvRange) = derivativeRanges
  Segment
    { range = range uvRange surface
    , partialDerivativeRanges = derivativeRanges
    , secondPartialDerivativeRanges = secondPartialDerivativeRanges uvRange surface
    , normalDirectionRange = normalDirectionRange uvRange surface
    , isDegenerate = isDegenerateLeft || isDegenerateRight || isDegenerateBottom || isDegenerateTop
    , isMonotonic = VectorBounds3D.areIndependent duRange dvRange
    }

on :: Tolerance Meters => Plane3D space -> Region2D Meters -> Result IsDegenerate (Surface3D space)
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

extruded ::
  Tolerance Meters =>
  Curve3D space ->
  Vector3D Meters space ->
  Result IsDegenerate (Surface3D space)
extruded curve displacement = translational curve (displacement * Curve1D.t)

translational ::
  Tolerance Meters =>
  Curve3D space ->
  VectorCurve3D Meters space ->
  Result IsDegenerate (Surface3D space)
translational baseCurve translationCurve = do
  let baseFunction = baseCurve << SurfaceFunction1D.u
  let translationFunction = translationCurve << SurfaceFunction1D.v
  let translationalFunction = baseFunction + translationFunction
  parametric translationalFunction UvRegion.unitSquare

ruled ::
  Tolerance Meters =>
  Curve3D space ->
  Curve3D space ->
  Result IsDegenerate (Surface3D space)
ruled bottom top = do
  let bottomFunction = bottom << SurfaceFunction1D.u
  let topFunction = top << SurfaceFunction1D.u
  let displacementFunction = SurfaceFunction1D.v * (topFunction - bottomFunction)
  let ruledFunction = bottomFunction + displacementFunction
  parametric ruledFunction UvRegion.unitSquare

revolved ::
  Tolerance Meters =>
  Plane3D space ->
  Curve2D Meters ->
  Axis2D Meters ->
  Angle ->
  Result IsDegenerate (Surface3D space)
revolved plane curve axis angle = do
  let frame2D = Frame2D.fromYAxis axis
  let localCurve = Curve2D.relativeTo frame2D curve
  let (xCoordinate, yCoordinate) = Curve2D.coordinates localCurve
  let frame3D = Frame3D.fromBackPlane (Frame2D.placeOn plane frame2D)
  let radius = xCoordinate << SurfaceFunction1D.u
  let height = yCoordinate << SurfaceFunction1D.u
  let theta = angle * SurfaceFunction1D.v
  let surfaceFunction =
        frame3D.originPoint
          + radius * SurfaceFunction1D.cos theta * Frame3D.rightwardDirection frame3D
          + radius * SurfaceFunction1D.sin theta * Frame3D.forwardDirection frame3D
          + height * Frame3D.upwardDirection frame3D
  parametric surfaceFunction UvRegion.unitSquare

bounds :: Surface3D space -> Bounds3D space
bounds surface = SurfaceFunction3D.range (Region2D.bounds surface.domain) surface.function

boundaryCurves :: Surface3D space -> Set3D space (SurfaceCurve3D space)
boundaryCurves = Set3D.flatten . boundaries

flip :: Surface3D space -> Surface3D space
flip surface = recursive \flippedSurface -> do
  let flippedFunction = SurfaceFunction3D.flip surface.function
  let flippedDomain = UvRegion.flip surface.domain
  let (flippedOuterBoundary, flippedInnerBoundaries) =
        -- TODO figure out a nicer approach here?
        -- In general buildBoundaries *should* take a tolerance and return a Result
        -- (and therefore ensure that all boundary curves are nondegenerate),
        -- but here we know that it should always succeed
        Tolerance.using Quantity.zero (buildBoundaries flippedFunction flippedDomain)
  Surface3D
    { function = flippedFunction
    , domain = flippedDomain
    , outerBoundary = flippedOuterBoundary
    , innerBoundaries = flippedInnerBoundaries
    , degenerateLeft = surface.degenerateRight
    , degenerateRight = surface.degenerateLeft
    , degenerateBottom = surface.degenerateBottom
    , degenerateTop = surface.degenerateTop
    , bisectionTree = buildBisectionTree UvBounds.unitSquare flippedSurface
    }

-- | Convert a surface defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3D global local -> Surface3D local -> Surface3D global
placeIn frame surface = recursive \placedSurface -> do
  let placeBoundary boundary = Set3D.map (SurfaceCurve3D.placeIn frame) boundary
  let placedOuterBoundary = placeBoundary surface.outerBoundary
  let placedInnerBoundaries = Bag3D.map placeBoundary surface.innerBoundaries
  Surface3D
    { function = SurfaceFunction3D.placeIn frame surface.function
    , domain = surface.domain
    , outerBoundary = placedOuterBoundary
    , innerBoundaries = placedInnerBoundaries
    , degenerateLeft = surface.degenerateLeft
    , degenerateRight = surface.degenerateRight
    , degenerateBottom = surface.degenerateBottom
    , degenerateTop = surface.degenerateTop
    , bisectionTree = buildBisectionTree UvBounds.unitSquare placedSurface
    }

-- | Convert a surface defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3D global local -> Surface3D global -> Surface3D local
relativeTo frame = placeIn (Frame3D.inverse frame)

findPoint :: Tolerance Meters => Point3D space -> Surface3D space -> List (SurfacePoint3D space)
findPoint = Surface3D.FindPoint.findPoint
