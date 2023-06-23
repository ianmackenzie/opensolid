module Bisection
  ( Domain
  , Tree
  , Quadtree
  , tree
  , quadtree
  , solve
  , solve2
  )
where

import Domain (Domain)
import Domain qualified
import OpenSolid
import Range qualified

data Cache segment = Cache Domain ~segment

data Cache2 segment = Cache2 Domain Domain ~segment

data Tree segment
  = Tree (Cache segment) (Cache segment) ~(Tree segment) ~(Tree segment)

data Quadtree segment
  = Quadtree
      (Cache2 segment)
      (Cache2 segment)
      ~(Quadtree segment)
      ~(Quadtree segment)
      ~(Quadtree segment)
      ~(Quadtree segment)

tree :: (Domain -> segment) -> Tree segment
tree compute = buildTree compute Domain.unit

buildTree :: (Domain -> segment) -> Domain -> Tree segment
buildTree compute domain =
  let cache = Cache domain (compute domain)
      neighborhoodDomain = Domain.expand domain
      neighborhoodCache = Cache neighborhoodDomain (compute neighborhoodDomain)
      (left, right) = Range.bisect domain
   in Tree cache neighborhoodCache (buildTree compute left) (buildTree compute right)

quadtree :: (Domain -> Domain -> segment) -> Quadtree segment
quadtree compute = buildQuadtree compute Domain.unit Domain.unit

buildQuadtree :: (Domain -> Domain -> segment) -> Domain -> Domain -> Quadtree segment
buildQuadtree compute xDomain yDomain =
  let cache = Cache2 xDomain yDomain (compute xDomain yDomain)
      xNeighborhood = Domain.expand xDomain
      yNeighborhood = Domain.expand yDomain
      neighborhoodCache = Cache2 xNeighborhood yNeighborhood (compute xNeighborhood yNeighborhood)
      (x1, x2) = Range.bisect xDomain
      (y1, y2) = Range.bisect yDomain
   in Quadtree
        cache
        neighborhoodCache
        (buildQuadtree compute x1 y1)
        (buildQuadtree compute x2 y1)
        (buildQuadtree compute x1 y2)
        (buildQuadtree compute x2 y2)

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

solve2
  :: (Domain -> Domain -> segment -> Bool)
  -> (Domain -> Domain -> segment -> Fuzzy resolution)
  -> (Domain -> Domain -> segment -> resolution -> Maybe solution)
  -> Quadtree segment
  -> (List solution, List (Domain, Domain))
  -> (List solution, List (Domain, Domain))
solve2 isCandidate resolveNeighborhood findSolution segmentTree accumulated =
  case isExcludedBy2 exclusions xDomain yDomain of
    Resolved True -> accumulated
    Resolved False -> search2 isCandidate resolveNeighborhood findSolution segmentTree accumulated
    Unresolved ->
      if isCandidate xDomain yDomain segment
        then
          accumulated
            |> solve2 isCandidate resolveNeighborhood findSolution child4
            |> solve2 isCandidate resolveNeighborhood findSolution child3
            |> solve2 isCandidate resolveNeighborhood findSolution child2
            |> solve2 isCandidate resolveNeighborhood findSolution child1
        else accumulated
 where
  Quadtree cache _ child1 child2 child3 child4 = segmentTree
  (Cache2 xDomain yDomain segment) = cache
  (_, exclusions) = accumulated

isExcludedBy :: List Domain -> Domain -> Fuzzy Bool
isExcludedBy [] _ = Resolved False
isExcludedBy (first : rest) domain
  | Range.intersects domain first =
      if Range.contains domain first
        then Resolved True
        else Unresolved
  | otherwise = isExcludedBy rest domain

isExcludedBy2 :: List (Domain, Domain) -> Domain -> Domain -> Fuzzy Bool
isExcludedBy2 [] _ _ = Resolved False
isExcludedBy2 ((firstX, firstY) : rest) xDomain yDomain
  | Range.intersects xDomain firstX && Range.intersects yDomain firstY =
      if Range.contains xDomain firstX && Range.contains yDomain firstY
        then Resolved True
        else Unresolved
  | otherwise = isExcludedBy2 rest xDomain yDomain

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

search2
  :: (Domain -> Domain -> segment -> Bool)
  -> (Domain -> Domain -> segment -> Fuzzy resolution)
  -> (Domain -> Domain -> segment -> resolution -> Maybe solution)
  -> Quadtree segment
  -> (List solution, List (Domain, Domain))
  -> (List solution, List (Domain, Domain))
search2 isCandidate resolveNeighborhood findSolution segmentTree accumulated =
  if isCandidate xDomain yDomain segment
    then case resolveNeighborhood xNeighborhood yNeighborhood neighborhood of
      Resolved resolution ->
        case findSolution xDomain yDomain segment resolution of
          Just solution -> (solution : solutions, (xNeighborhood, yNeighborhood) : exclusions)
          Nothing -> accumulated
      Unresolved ->
        accumulated
          |> search2 isCandidate resolveNeighborhood findSolution child4
          |> search2 isCandidate resolveNeighborhood findSolution child3
          |> search2 isCandidate resolveNeighborhood findSolution child2
          |> search2 isCandidate resolveNeighborhood findSolution child1
    else accumulated
 where
  Quadtree cache neighborhoodCache child1 child2 child3 child4 = segmentTree
  (Cache2 xDomain yDomain segment) = cache
  (Cache2 xNeighborhood yNeighborhood neighborhood) = neighborhoodCache
  (solutions, exclusions) = accumulated
