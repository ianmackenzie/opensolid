{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Surface3D.FindPoint (findPoint) where

import OpenSolid.Bag qualified as Bag
import OpenSolid.Bag3D qualified as Bag3D
import OpenSolid.Bisection qualified as Bisection
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson.Surface qualified as NewtonRaphson.Surface
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Queue qualified as Queue
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Surface3D (Surface3D)
import OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.Surface3D.Segment qualified as Segment
import OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.SurfacePoint3D (SurfacePoint3D)
import OpenSolid.SurfacePoint3D qualified as SurfacePoint3D
import OpenSolid.UvBag (UvBag)
import OpenSolid.UvBag qualified as UvBag
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvCurve qualified as UvCurve
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.UvRegion qualified as UvRegion
import OpenSolid.VectorSurfaceFunction3D qualified as VectorSurfaceFunction3D

findPoint :: Tolerance Meters => Point3D space -> Surface3D space -> List (SurfacePoint3D space)
findPoint point surface
  | not (point ^ Surface3D.bounds surface) = []
  | otherwise =
      case findBoundarySolutions point surface of
        NonEmpty boundarySolutions -> NonEmpty.toList boundarySolutions
        [] -> List.maybe (findInteriorSolution point surface)

findBoundarySolutions ::
  Tolerance Meters =>
  Point3D space ->
  Surface3D space ->
  List (SurfacePoint3D space)
findBoundarySolutions point surface = do
  let poleSolutions =
        Surface3D.poles surface -- Get all surface poles
          & Bag3D.filter (^ point) (^ point) -- Filter down to the ones that touch the given point
          & Bag.project @UvBounds -- Convert to a UV-space set
  let edgeSolutions =
        Surface3D.edges surface -- Get all surface edges
          & Bag3D.filterBounds (^ point) -- Filter out ones that definitely don't touch the given point
          & List.combine (findEdgeSolutions poleSolutions point) -- Try to find the given point on each edge
          & UvBag.pack -- Pack into a bag to find duplicates efficiently
          & UvBag.uniqueItems -- Filter out duplicate solutions
  let toSurfacePoint uvPoint = SurfacePoint3D.Point uvPoint point
  UvBag.toListOf SurfacePoint3D.Pole poleSolutions <> List.map toSurfacePoint edgeSolutions

findEdgeSolutions ::
  Tolerance Meters =>
  UvBag (Surface3D.Pole space) ->
  Point3D space ->
  Surface3D.Edge space ->
  List UvPoint
findEdgeSolutions poleSolutions point (Surface3D.Edge uvCurve curve) =
  Curve3D.findPoint point curve
    & List.filterMap \tValue -> do
      let uvPoint = UvCurve.pointAt tValue uvCurve
      if unitless (uvPoint ^ poleSolutions) then Nothing else Just uvPoint

data UniqueSolution = UniqueSolution deriving (Eq)

findInteriorSolution ::
  Tolerance Meters =>
  Point3D space ->
  Surface3D space ->
  Maybe (SurfacePoint3D space)
findInteriorSolution point surface = do
  let (du, dv) = SurfaceFunction3D.partialDerivatives (Surface3D.function surface)
  let evaluateNewtonRaphson uvPoint = do
        let displacement = Surface3D.pointAt uvPoint surface - point
        let duValue = VectorSurfaceFunction3D.valueAt uvPoint du
        let dvValue = VectorSurfaceFunction3D.valueAt uvPoint dv
        (# displacement, duValue, dvValue #)
  let domain = Surface3D.domain surface
  let isDistant segment = not (point ^ Segment.range segment)
  let isExterior uvRange =
        UvRegion.classifyBounds uvRange domain == Resolved Region2D.ExteriorBounds
  let resolvedUniqueness uvRange segment
        | isDistant segment || isExterior uvRange = Resolved Nothing
        | Segment.isMonotonic segment = Resolved (Just UniqueSolution)
        | Segment.isDegenerate segment = Resolved (Just UniqueSolution)
        | otherwise = Unresolved
  let candidates =
        Surface3D.bisectionTree surface
          & Bisection.resolve UvBag.empty resolvedUniqueness
  let isSolution uvPoint = Surface3D.pointAt uvPoint surface ~= point
  let isInterior uvPoint = UvRegion.classifyPoint uvPoint domain == Region2D.InteriorPoint
  let isInteriorSolution uvPoint = isSolution uvPoint && isInterior uvPoint
  let resolvedSolution UniqueSolution uvRange segment
        | isDistant segment || isExterior uvRange = Resolved Nothing
        | otherwise = do
            uvPoint <- NewtonRaphson.Surface.solveIn uvRange evaluateNewtonRaphson
            let surfacePoint = SurfacePoint3D.Point uvPoint point
            Resolved (if isInteriorSolution uvPoint then Just surfacePoint else Nothing)
  Bisection.find resolvedSolution (Queue.fromBag candidates)
