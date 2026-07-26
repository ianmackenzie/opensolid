module OpenSolid.Intersection (Problem (..), solveNonOverlapping) where

import OpenSolid.Bag (Bag)
import OpenSolid.Bag qualified as Bag
import OpenSolid.Bisection (Tree)
import OpenSolid.Bisection qualified as Bisection
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import OpenSolid.SearchDomain qualified as SearchDomain
import OpenSolid.Set (Set)

data Problem intersection where
  Problem ::
    SearchDomain.Bounds subdomain =>
    { boundaryIntersections :: List intersection
    , boundaryTangentSubdomains :: Bag subdomain boundarySegment
    , boundaryCrossingSubdomains :: Bag subdomain boundarySegment
    , searchTree :: Tree subdomain segment
    , resolveTangent :: subdomain -> segment -> Fuzzy (Maybe tangentTag)
    , resolveCrossing :: subdomain -> segment -> Fuzzy (Maybe crossingTag)
    , solveTangent :: Set subdomain (tangentTag, Tree subdomain segment) -> List intersection
    , solveCrossing :: Set subdomain (crossingTag, Tree subdomain segment) -> List intersection
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
          allTangentClusters & List.filter (not . touching boundaryTangentSubdomains)
    let tangentIntersections = List.combine solveTangent candidateTangentClusters
    let tangentExclusions = Bag.aggregate (List.map Bag.full allTangentClusters)
    let allCrossingClusters = Bisection.clusters tangentExclusions resolveCrossing searchTree
    let candidateCrossingClusters =
          allCrossingClusters & List.filter (not . touching boundaryCrossingSubdomains)
    let crossingIntersections = List.combine solveCrossing candidateCrossingClusters
    List.concat [boundaryIntersections, tangentIntersections, crossingIntersections]

touching :: SearchDomain.Bounds subdomain => Bag subdomain a -> Set subdomain b -> Bool
touching bag set = Bag.full set & Bag.pairwiseAny (^) (\_ _ -> True) bag
