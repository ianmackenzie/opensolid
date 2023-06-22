module Bisection
  ( Domain
  , Tree
  , tree
  , solve
  )
where

import Domain (Domain)
import Domain qualified
import OpenSolid
import Range qualified

data Cache segment = Cache Domain ~segment

data Tree segment
  = Tree (Cache segment) (Cache segment) ~(Tree segment) ~(Tree segment)

tree :: (Domain -> segment) -> Tree segment
tree compute = buildTree compute Domain.unit

buildTree :: (Domain -> segment) -> Domain -> Tree segment
buildTree compute domain =
  let cache = Cache domain (compute domain)
      neighborhoodDomain = Domain.expand domain
      neighborhoodCache = Cache neighborhoodDomain (compute neighborhoodDomain)
      (left, right) = Range.bisect domain
   in Tree cache neighborhoodCache (buildTree compute left) (buildTree compute right)

solve
  :: (Domain -> segment -> Bool)
  -> (Domain -> segment -> Fuzzy resolution)
  -> (Domain -> segment -> resolution -> Maybe solution)
  -> Tree segment
  -> (List solution, List Domain)
  -> (List solution, List Domain)
solve isCandidate resolveNeighborhood findSolution segmentTree accumulated =
  case isExcludedBy exclusions domain of
    Resolved True -> accumulated
    Resolved False -> search isCandidate resolveNeighborhood findSolution segmentTree accumulated
    Unresolved ->
      if isCandidate domain segment
        then
          accumulated
            |> solve isCandidate resolveNeighborhood findSolution rightChild
            |> solve isCandidate resolveNeighborhood findSolution leftChild
        else accumulated
 where
  Tree cache _ leftChild rightChild = segmentTree
  (Cache domain segment) = cache
  (_, exclusions) = accumulated

isExcludedBy :: List Domain -> Domain -> Fuzzy Bool
isExcludedBy [] _ = Resolved False
isExcludedBy (first : rest) domain
  | Range.intersects domain first =
      if Range.contains domain first
        then Resolved True
        else Unresolved
  | otherwise = isExcludedBy rest domain

search
  :: (Domain -> segment -> Bool)
  -> (Domain -> segment -> Fuzzy resolution)
  -> (Domain -> segment -> resolution -> Maybe solution)
  -> Tree segment
  -> (List solution, List Domain)
  -> (List solution, List Domain)
search isCandidate resolveNeighborhood findSolution segmentTree accumulated =
  if isCandidate domain segment
    then case resolveNeighborhood expandedDomain neighborhood of
      Resolved resolution ->
        case findSolution domain segment resolution of
          Just solution -> (solution : solutions, expandedDomain : exclusions)
          Nothing -> accumulated
      Unresolved ->
        accumulated
          |> search isCandidate resolveNeighborhood findSolution rightChild
          |> search isCandidate resolveNeighborhood findSolution leftChild
    else accumulated
 where
  Tree cache neighborhoodCache leftChild rightChild = segmentTree
  (Cache domain segment) = cache
  (Cache expandedDomain neighborhood) = neighborhoodCache
  (solutions, exclusions) = accumulated
