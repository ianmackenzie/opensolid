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
  let allowed = isAllowed domain exclusions
      candidate = isCandidate domain segment
      resolved = resolveNeighborhood expandedDomain neighborhood
      recurse =
        accumulated
          |> solve isCandidate resolveNeighborhood findSolution rightChild
          |> solve isCandidate resolveNeighborhood findSolution leftChild
   in case (allowed, candidate, resolved) of
        (Resolved False, _, _) -> accumulated
        (_, False, _) -> accumulated
        (Unresolved, True, _) -> recurse
        (Resolved True, True, Unresolved) -> recurse
        (Resolved True, True, Resolved resolution) ->
          case findSolution domain segment resolution of
            Just solution -> (solution : solutions, expandedDomain : exclusions)
            Nothing -> accumulated
 where
  Tree cache neighborhoodCache leftChild rightChild = segmentTree
  (Cache domain segment) = cache
  (Cache expandedDomain neighborhood) = neighborhoodCache
  (solutions, exclusions) = accumulated

isAllowed :: Domain -> List Domain -> Fuzzy Bool
isAllowed _ [] = Resolved True
isAllowed domain (first : rest)
  | Range.intersects domain first =
      if Range.contains domain first
        then Resolved False
        else Unresolved
  | otherwise = isAllowed domain rest

solve2
  :: Show solution
  => (Domain -> Domain -> segment -> Bool)
  -> (Domain -> Domain -> segment -> Fuzzy resolution)
  -> (Domain -> Domain -> segment -> resolution -> Maybe solution)
  -> Quadtree segment
  -> (List solution, List (Domain, Domain))
  -> (List solution, List (Domain, Domain))
solve2 isCandidate resolveNeighborhood findSolution segmentTree accumulated =
  let allowed = isAllowed2 xDomain yDomain exclusions
      candidate = isCandidate xDomain yDomain segment
      resolved = resolveNeighborhood xNeighborhood yNeighborhood neighborhood
      recurse =
        accumulated
          |> solve2 isCandidate resolveNeighborhood findSolution child4
          |> solve2 isCandidate resolveNeighborhood findSolution child3
          |> solve2 isCandidate resolveNeighborhood findSolution child2
          |> solve2 isCandidate resolveNeighborhood findSolution child1
   in case (allowed, candidate, resolved) of
        (Resolved False, _, _) -> accumulated
        (_, False, _) -> accumulated
        (Unresolved, True, _) -> recurse
        (Resolved True, True, Unresolved) -> recurse
        (Resolved True, True, Resolved resolution) ->
          case findSolution xDomain yDomain segment resolution of
            Just solution -> (solution : solutions, (xNeighborhood, yNeighborhood) : exclusions)
            Nothing -> accumulated
 where
  Quadtree cache neighborhoodCache child1 child2 child3 child4 = segmentTree
  (Cache2 xDomain yDomain segment) = cache
  (Cache2 xNeighborhood yNeighborhood neighborhood) = neighborhoodCache
  (solutions, exclusions) = accumulated

isAllowed2 :: Domain -> Domain -> List (Domain, Domain) -> Fuzzy Bool
isAllowed2 _ _ [] = Resolved True
isAllowed2 xDomain yDomain ((firstX, firstY) : rest)
  | Range.intersects xDomain firstX && Range.intersects yDomain firstY =
      if Range.contains xDomain firstX && Range.contains yDomain firstY
        then Resolved False
        else Unresolved
  | otherwise = isAllowed2 xDomain yDomain rest
