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
import OpenSolid.Pair qualified as Pair
import OpenSolid.Point (Point)
import OpenSolid.Point qualified as Point
import OpenSolid.Prelude
import OpenSolid.Search qualified as Search
import OpenSolid.Search.Domain as Domain
import OpenSolid.Vector qualified as Vector
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve

type Tree dimension units space =
  Search.Tree (Interval Unitless) (Segment dimension units space)

tree ::
  ( Curve.Exists dimension units space
  , VectorCurve.Exists dimension units space
  , DirectionCurve.Exists dimension space
  , Tolerance units
  ) =>
  Curve dimension units space ->
  Result Curve.IsPoint (Tree dimension units space)
tree givenCurve = do
  computedTangentCurve <- Curve.tangentDirection givenCurve
  Ok (Search.tree (Segment.evaluate givenCurve computedTangentCurve) Search.curveDomain)

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
  let displacementFunction tValue = Curve.evaluate searchCurve tValue - point
  let derivativeFunction = VectorCurve.evaluate (Curve.derivative searchCurve)
  let startIsSolution = Curve.startPoint searchCurve ~= point
  let startIsSingular = derivativeFunction 0.0 ~= Vector.zero
  let endIsSolution = Curve.endPoint searchCurve ~= point
  let endIsSingular = derivativeFunction 1.0 ~= Vector.zero
  let callback tBounds segment
        | not (point `intersects` Segment.bounds segment) =
            Resolved Nothing
        | Interval.lower tBounds == 0.0
        , startIsSolution
        , Segment.monotonic segment || (startIsSingular && Domain.isSmall tBounds) =
            Resolved (Just 0.0)
        | Interval.upper tBounds == 1.0
        , endIsSolution
        , Segment.monotonic segment || (endIsSingular && Domain.isSmall tBounds) =
            Resolved (Just 1.0)
        | Segment.monotonic segment
        , let tMid = Interval.midpoint tBounds
        , let tSolution = NewtonRaphson.curve displacementFunction derivativeFunction tMid
        , Interval.includes tSolution (Domain.interior tBounds)
        , Curve.evaluate (Segment.curve segment) tSolution ~= point =
            Resolved (Just tSolution)
        | otherwise =
            Unresolved
  let solutions = Search.exclusive callback searchTree
  List.map Pair.second (deduplicate solutions [])

deduplicate ::
  List (Interval Unitless, Number) ->
  List (Interval Unitless, Number) ->
  List (Interval Unitless, Number)
deduplicate [] accumulated = accumulated
deduplicate (first : rest) accumulated =
  if List.anySatisfy (isDuplicate first) accumulated
    then deduplicate rest accumulated
    else deduplicate rest (first : accumulated)

isDuplicate :: (Interval Unitless, Number) -> (Interval Unitless, Number) -> Bool
isDuplicate (tBounds1, _) (tBounds2, _) = Domain.overlapping tBounds1 tBounds2
