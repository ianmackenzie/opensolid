module OpenSolid.Curve.Search
  ( Tree
  , tree
  , curve
  , tangentCurve
  , findPoint
  )
where

import OpenSolid.Bounds qualified as Bounds
import {-# SOURCE #-} OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Segment (Segment)
import OpenSolid.Curve.Segment qualified as Segment
import {-# SOURCE #-} OpenSolid.DirectionCurve (DirectionCurve)
import {-# SOURCE #-} OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.Number qualified as Number
import OpenSolid.Pair qualified as Pair
import OpenSolid.Point (Point)
import OpenSolid.Point qualified as Point
import OpenSolid.Prelude
import OpenSolid.Search qualified as Search
import OpenSolid.Search.Domain qualified as Search.Domain
import OpenSolid.Vector qualified as Vector
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve

type Tree dimension units space =
  Search.Tree (Interval Unitless) (Segment dimension units space)

tree ::
  ( Curve.Exists dimension units space
  , VectorCurve.Exists dimension units space
  , DirectionCurve.Exists dimension space
  , VectorCurve.Exists dimension (Unitless ?/? units) space
  , Tolerance units
  ) =>
  Curve dimension units space ->
  Result Curve.IsPoint (Tree dimension units space)
tree givenCurve = do
  computedTangentCurve <- Curve.tangentDirection givenCurve
  computedCurvatureVectorCurve_ <- Curve.curvatureVector_ givenCurve
  let evaluateSegment tBounds =
        Segment.evaluate givenCurve computedTangentCurve computedCurvatureVectorCurve_ tBounds
  Ok (Search.tree evaluateSegment Search.curveDomain)

curve :: Tree dimension units space -> Curve dimension units space
curve (Search.Tree _ segment _) = Segment.curve segment

tangentCurve :: Tree dimension units space -> DirectionCurve dimension space
tangentCurve (Search.Tree _ segment _) = Segment.tangentCurve segment

findPoint ::
  ( Point.Exists dimension units space
  , Bounds.Exists dimension units space
  , Curve.Exists dimension units space
  , VectorCurve.Exists dimension units space
  , NewtonRaphson.Curve dimension units space
  , Tolerance units
  ) =>
  Point dimension units space ->
  Tree dimension units space ->
  List Number
findPoint point searchTree = do
  let searchCurve = curve searchTree
  let curveDerivative = Curve.derivative searchCurve
  let displacement tValue = Curve.evaluate searchCurve tValue - point
  let displacementDerivative tValue = VectorCurve.evaluate curveDerivative tValue
  let isSolution tValue = Curve.evaluate searchCurve tValue ~= point
  let isDegenerate tValue = VectorCurve.evaluate curveDerivative tValue ~= Vector.zero
  let endpointSolutions = List.filter isSolution [0.0, 1.0]
  let solveMonotonic tBounds = do
        let tMid = Interval.midpoint tBounds
        let tSolution = NewtonRaphson.curve displacement displacementDerivative tMid
        if Interval.includes tSolution (Search.Domain.interior tBounds) && isSolution tSolution
          then Resolved (Just tSolution)
          else Unresolved
  let interiorSolution tBounds segment
        | not (point `intersects` Segment.bounds segment) = Resolved Nothing
        | otherwise = do
            let isMonotonic = Segment.monotonic segment
            let isSmall = Search.Domain.isSmall tBounds
            let endpointSolution = List.find (Number.includedIn tBounds) endpointSolutions
            let hasEndpointSolution = endpointSolution /= Nothing
            if
              | isMonotonic, hasEndpointSolution -> Resolved Nothing
              | isSmall, Just tValue <- endpointSolution, isDegenerate tValue -> Resolved Nothing
              | isMonotonic -> solveMonotonic tBounds
              | otherwise -> Unresolved
  let isDuplicate (tBounds1, _) (tBounds2, _) = Search.Domain.overlapping tBounds1 tBounds2
  let interiorSolutions = List.map Pair.second (Search.exclusive interiorSolution isDuplicate searchTree)
  List.sort (endpointSolutions <> interiorSolutions)
