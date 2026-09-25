{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Surface3D.FindPoint (findPoint) where

import OpenSolid.Bag qualified as Bag
import OpenSolid.Bag3D (Bag3D)
import OpenSolid.Bag3D qualified as Bag3D
import OpenSolid.Bisection qualified as Bisection
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson.Surface qualified as NewtonRaphson.Surface
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Surface3D (Surface3D)
import OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.Surface3D.Segment qualified as Segment
import OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.SurfacePoint3D (SurfacePoint3D)
import OpenSolid.SurfacePoint3D qualified as SurfacePoint3D
import OpenSolid.UvCurve qualified as UvCurve
import OpenSolid.UvRegion qualified as UvRegion
import OpenSolid.VectorSurfaceFunction3D qualified as VectorSurfaceFunction3D

data UniqueSolution = UniqueSolution deriving (Eq)

findPoint :: Tolerance Meters => Point3D space -> Surface3D space -> List (SurfacePoint3D space)
findPoint point surface = do
  let domain = Surface3D.domain surface
  -- Find boundary solutions
  let vertexSolutions = Surface3D.vertices surface & Bag3D.filter (^ point) (^ point)
  let edgeSolutions =
        Surface3D.edges surface
          & Bag3D.filterBounds (^ point)
          & Bag3D.combine (findInteriorEdgePoints point)
  let boundarySolutions = vertexSolutions <> edgeSolutions
  let boundaryExclusions = Bag.map SurfacePoint3D.uvBounds boundarySolutions
  -- Define Newton-Raphson evaluator for interior solutions
  let (du, dv) = SurfaceFunction3D.partialDerivatives (Surface3D.function surface)
  let evaluateNewtonRaphson uvPoint = do
        let displacement = Surface3D.pointAt uvPoint surface - point
        let duValue = VectorSurfaceFunction3D.valueAt uvPoint du
        let dvValue = VectorSurfaceFunction3D.valueAt uvPoint dv
        (# displacement, duValue, dvValue #)
  -- Find interior solutions
  let isDistant segment = not (point ^ Segment.range segment)
  let isExterior uvRange = UvRegion.classifyBounds uvRange domain == Resolved Region2D.Outside
  let resolvedUniqueness uvRange segment
        | isDistant segment || isExterior uvRange = Resolved Nothing
        | Segment.isMonotonic segment = Resolved (Just UniqueSolution)
        | Segment.isDegenerate segment = Resolved (Just UniqueSolution)
        | otherwise = Unresolved
  let isSolution uvPoint = Surface3D.pointAt uvPoint surface ~= point
  let isInterior uvPoint = UvRegion.classify uvPoint domain == Region2D.Inside
  let isInteriorSolution uvPoint = isSolution uvPoint && isInterior uvPoint
  let resolvedSolution UniqueSolution uvRange segment
        | isDistant segment || isExterior uvRange = Resolved Nothing
        | Resolved uvPoint <- NewtonRaphson.Surface.solveIn uvRange evaluateNewtonRaphson =
            Resolved $
              if isInteriorSolution uvPoint
                then Just (SurfacePoint3D.Point uvPoint point)
                else Nothing
        | otherwise = Unresolved
  let interiorSolutions =
        Surface3D.bisectionTree surface
          & Bisection.clusters boundaryExclusions resolvedUniqueness
          & List.filterMap (Bisection.find resolvedSolution)
  -- Return all solutions (although should be rare to have both!)
  Bag3D.toList boundarySolutions <> interiorSolutions

findInteriorEdgePoints ::
  Tolerance Meters =>
  Point3D space ->
  Surface3D.Edge space ->
  Bag3D space (SurfacePoint3D space)
findInteriorEdgePoints point (Surface3D.Edge uvCurve curve) = do
  let tValues = Curve3D.findPoint point curve
  let innerTValues = List.filter (not . Parameter.isEndpoint) tValues
  let toPoint tValue = SurfacePoint3D.Point (UvCurve.pointAt tValue uvCurve) point
  Bag3D.pack (List.map toPoint innerTValues)
