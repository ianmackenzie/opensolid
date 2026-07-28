module OpenSolid.Intersection (Problem (..), solveNonOverlapping) where

import OpenSolid.Bag (Bag)
import OpenSolid.Bag qualified as Bag
import OpenSolid.Bisection (Tree)
import OpenSolid.Bisection qualified as Bisection
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import OpenSolid.Set (Set)

data Problem intersection where
  Problem ::
    Bisection.Domain domain =>
    { boundaryIntersections :: List intersection
    , boundaryTangentSubdomains :: Bag domain boundarySegment
    , boundaryCrossingSubdomains :: Bag domain boundarySegment
    , searchTree :: Tree domain segment
    , resolveTangent :: domain -> segment -> Fuzzy (Maybe tangentTag)
    , resolveCrossing :: domain -> segment -> Fuzzy (Maybe crossingTag)
    , solveTangent :: Set domain (tangentTag, Tree domain segment) -> List intersection
    , solveCrossing :: Set domain (crossingTag, Tree domain segment) -> List intersection
    } ->
    Problem intersection

solveNonOverlapping :: Problem intersection -> List intersection
solveNonOverlapping
  Problem
    { boundaryIntersections
    , boundaryTangentSubdomains
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
    let tangentIntersections = List.combine solveTangent candidateTangentClusters
    let tangentExclusions = Bag.aggregate (List.map Bag.full allTangentClusters)
    let allCrossingClusters = Bisection.clusters tangentExclusions resolveCrossing searchTree
    let candidateCrossingClusters =
          allCrossingClusters & List.filter (not . Bisection.touching boundaryCrossingSubdomains)
    let crossingIntersections = List.combine solveCrossing candidateCrossingClusters
    List.concat [boundaryIntersections, tangentIntersections, crossingIntersections]
