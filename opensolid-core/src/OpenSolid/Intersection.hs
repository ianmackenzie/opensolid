module OpenSolid.Intersection (Problem (..), solveInterior) where

import OpenSolid.Bag (Bag)
import OpenSolid.Bag qualified as Bag
import OpenSolid.Bisection (Tree)
import OpenSolid.Bisection qualified as Bisection
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import OpenSolid.Queue (Queue)
import OpenSolid.Queue qualified as Queue

data Problem intersection where
  Problem ::
    Bisection.Domain domain =>
    { boundaryTangentSubdomains :: Bag domain boundarySegment
    , boundaryCrossingSubdomains :: Bag domain boundarySegment
    , searchTree :: Tree domain segment
    , resolveTangent :: domain -> segment -> Fuzzy (Maybe tangentTag)
    , resolveCrossing :: domain -> segment -> Fuzzy (Maybe crossingTag)
    , solveTangent :: Queue (Bisection.Subtree tangentTag domain segment) -> List intersection
    , solveCrossing :: Queue (Bisection.Subtree crossingTag domain segment) -> List intersection
    } ->
    Problem intersection

solveInterior :: Problem intersection -> List intersection
solveInterior
  Problem
    { boundaryTangentSubdomains
    , boundaryCrossingSubdomains
    , searchTree
    , resolveTangent
    , solveTangent
    , resolveCrossing
    , solveCrossing
    } = do
    let allTangentClusters = Bisection.clusters Bag.empty resolveTangent searchTree
    let candidateTangentClusters =
          allTangentClusters & List.filter (not . Bisection.touching boundaryTangentSubdomains)
    let tangentIntersections = List.combine (solveTangent . Queue.fromSet) candidateTangentClusters
    let tangentExclusions = Bag.aggregate (List.map Bag.full allTangentClusters)
    let allCrossingClusters = Bisection.clusters tangentExclusions resolveCrossing searchTree
    let candidateCrossingClusters =
          allCrossingClusters & List.filter (not . Bisection.touching boundaryCrossingSubdomains)
    let crossingIntersections = List.combine (solveCrossing . Queue.fromSet) candidateCrossingClusters
    List.concat [tangentIntersections, crossingIntersections]
