{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Surface3D.Nondegenerate.FindPoint (findPoint) where

import OpenSolid.Bag qualified as Bag
import OpenSolid.Bag3D (Bag3D)
import OpenSolid.Bag3D qualified as Bag3D
import OpenSolid.Bisection qualified as Bisection
import OpenSolid.Curve.Nondegenerate qualified as Curve.Nondegenerate
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson.Surface qualified as NewtonRaphson.Surface
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Surface3D (Surface3D)
import OpenSolid.Surface3D.Nondegenerate qualified as Surface3D.Nondegenerate
import OpenSolid.SurfaceCurve3D (SurfaceCurve3D)
import OpenSolid.SurfaceCurve3D.Nondegenerate qualified as SurfaceCurve3D.Nondegenerate
import OpenSolid.SurfaceFunction3D.Nondegenerate qualified as SurfaceFunction3D.Nondegenerate
import OpenSolid.SurfaceFunction3D.Segment qualified as Segment
import OpenSolid.SurfacePoint3D (SurfacePoint3D)
import OpenSolid.SurfacePoint3D qualified as SurfacePoint3D
import OpenSolid.UvRegion qualified as UvRegion
import OpenSolid.VectorSurfaceFunction3D.Nondegenerate qualified as VectorSurfaceFunction3D.Nondegenerate

data UniqueSolution = UniqueSolution deriving (Eq)

findPoint ::
  Tolerance Meters =>
  Point3D space ->
  Nondegenerate (Surface3D space) ->
  List (SurfacePoint3D space)
findPoint point surface = do
  let function = Surface3D.Nondegenerate.function surface
  let domain = Surface3D.Nondegenerate.domain surface
  -- Find boundary solutions
  let vertexSolutions =
        Surface3D.Nondegenerate.vertices surface
          & Bag3D.filter (^ point) (^ point)
  let edgeSolutions =
        Surface3D.Nondegenerate.edges surface
          & Bag3D.cull (^ point)
          & Bag3D.combine (findInteriorEdgePoints point)
  let boundarySolutions = vertexSolutions <> edgeSolutions
  let boundaryExclusions = Bag.map SurfacePoint3D.uvBounds boundarySolutions
  -- Define Newton-Raphson evaluator for interior solutions
  let (du, dv) = SurfaceFunction3D.Nondegenerate.partialDerivatives function
  let evaluateNewtonRaphson uvPoint = do
        let displacement = SurfaceFunction3D.Nondegenerate.pointAt uvPoint function - point
        let duValue = VectorSurfaceFunction3D.Nondegenerate.valueAt uvPoint du
        let dvValue = VectorSurfaceFunction3D.Nondegenerate.valueAt uvPoint dv
        (# displacement, duValue, dvValue #)
  -- Find interior solutions
  let isDistant segment = not (point ^ Segment.range segment)
  let isExterior uvRange = UvRegion.classifyBounds uvRange domain == Resolved Region2D.Outside
  let resolvedUniqueness uvRange segment
        | isDistant segment || isExterior uvRange = Resolved Nothing
        | Segment.isMonotonic segment = Resolved (Just UniqueSolution)
        | Segment.isDegenerate segment = Resolved (Just UniqueSolution)
        | otherwise = Unresolved
  let isSolution uvPoint = SurfaceFunction3D.Nondegenerate.pointAt uvPoint function ~= point
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
        SurfaceFunction3D.Nondegenerate.bisectionTree function
          & Bisection.clusters boundaryExclusions resolvedUniqueness
          & List.filterMap (Bisection.find resolvedSolution)
  -- Return all solutions (although should be rare to have both!)
  Bag3D.toList boundarySolutions <> interiorSolutions

findInteriorEdgePoints ::
  Tolerance Meters =>
  Point3D space ->
  Nondegenerate (SurfaceCurve3D space) ->
  Bag3D space (SurfacePoint3D space)
findInteriorEdgePoints point edge = do
  let curve = SurfaceCurve3D.Nondegenerate.curve edge
  let uvCurve = SurfaceCurve3D.Nondegenerate.uvCurve edge
  let tValues = Curve.Nondegenerate.findPoint point curve
  let innerTValues = List.filter (not . Parameter.isEndpoint) tValues
  let toPoint tValue = SurfacePoint3D.Point (Curve.Nondegenerate.pointAt tValue uvCurve) point
  Bag3D.pack (List.map toPoint innerTValues)
